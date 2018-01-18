-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Midi where
  import Data.Bifunctor
  import Data.ByteString
  import Data.Word
  type Channel = Word8
  data Composition = Composition Tempo [Melody_track] [Percussion_event] deriving Show
  type Err t = Either String t
  data Event = End | Start deriving Show
  data Event' = Event' Time Event Note deriving Show
  type File_name = String
  data Melodic_instrument =
    Bells |
    Cello |
    Contrabass |
    Flute |
    Guitar |
    Harp |
    Harpsichord |
    Piano |
    Strings |
    Viola |
    Violin |
    Voice
      deriving Show
  data Melody_track = Melody_track Melodic_instrument [Event'] deriving Show
  type Note = Word8
  data Percussion_event = Percussion_event Time Percussion_instrument deriving Show
  data Percussion_instrument =
    Bass_drum_1 |
    Bass_drum_2 |
    Cabasa |
    Chinese_cymbal |
    Claves |
    Closed_hi_hat |
    Cowbell |
    Crash_cymbal_1 |
    Crash_cymbal_2 |
    Hand_clap |
    High_agogo |
    High_bongo |
    High_timbale |
    High_tom_1 |
    High_tom_2 |
    High_wood_block |
    Long_guiro |
    Long_whistle |
    Low_agogo |
    Low_bongo |
    Low_conga |
    Low_timbale |
    Low_tom_1 |
    Low_tom_2 |
    Low_wood_block |
    Maracas |
    Mid_tom_1 |
    Mid_tom_2 |
    Mute_cuica |
    Mute_high_conga |
    Mute_triangle |
    Open_cuica |
    Open_hi_hat |
    Open_high_conga |
    Open_triangle |
    Pedal_hi_hat |
    Ride_bell |
    Ride_cymbal_1 |
    Ride_cymbal_2 |
    Short_guiro |
    Short_whistle |
    Side_stick |
    Snare_drum_1 |
    Snare_drum_2 |
    Splash_cymbal |
    Tambourine |
    Vibra_slap
      deriving (Eq, Show)
  type Tempo = Word32
  data Track = Track Channel [Word8] [Event']
  type Tracks = Word8
  type Time = Word32
  bytes :: Word8 -> String -> Word32 -> Err [Word8]
  bytes = bytes' []
  bytes' :: [Word8] -> Word8 -> String -> Word32 -> Err [Word8]
  bytes' x n err y =
    case n of
      0 ->
        case y of
          0 -> Right x
          _ -> Left err
      _ -> bytes' (fromIntegral (mod y 256) : x) (n - 1) err (div y 256) 
  encode :: Composition -> Err [Word8]
  encode (Composition tempo melody percussion) =
    (
      bytes 3 "Tempo too slow." tempo >>=
      \encoded_tempo ->
        (
          encode_melodies [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15] melody >>=
          \(tracks, encoded_melody) ->
            (++) [77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 0, tracks, 0, 8] <$> encode_tracks (Track 9 ([0, 255, 81, 3] ++ encoded_tempo) (encode_percussions (start <$> percussion)) : encoded_melody)))
  encode_event :: Channel -> Event' -> Err [Word8]
  encode_event channel (Event' time event note) =
    if note < 128
      then (++) [event_number event + channel, note, 127] <$> encode_time time
      else Left "The range of notes is {0,...,127}."
  encode_events :: Channel -> [Event'] -> Err [Word8]
  encode_events channel events =
    case events of
      [] -> Right []
      event : events' ->
        encode_event channel event >>= \encoded_event -> (++) encoded_event <$> encode_events channel events'
  encode_melodies :: [Channel] -> [Melody_track] -> Err (Tracks, [Track])
  encode_melodies channels melodies =
    case channels of
      [] -> Left "The number of melody tracks is limited to 15."
      channel : channels' ->
        case melodies of
          [] -> Right (1, [])
          (Melody_track instrument melody) : melodies' ->
            (
              bimap ((+) 1) ((:) (Track channel [0, 192 + channel, melodic_instrument instrument] melody)) <$>
              encode_melodies channels' melodies')
{-
  encode_percussion :: [Event'] -> Err [Word8]
  encode_percussion [] = Right []
  encode_percussion(Event' time event instrument : tl) = case encode_time time of
    Just encoded_time ->
      ((encoded_time ++ [event_number event + 9, instrument, 127]) ++) <$>
      encode_percussion(case event of
        End -> tl
        Start -> ins instrument 32 tl)
    Nothing -> Left "Event timing overflow."
-}
  encode_percussions :: [Event'] -> [Event']
  encode_percussions percussions =
    case percussions of
      [] -> []
      percussion @ (Event' time event instrument) : percussions' ->
        (
          percussion :
          encode_percussions
            (case event of
              End -> percussions'
              Start -> ins time instrument percussions'))
  encode_time :: Word32 -> Err [Word8]
  encode_time 0 = Right [0]
  encode_time time = encode_time' [] time
  encode_time' :: [Word8] -> Word32 -> Err [Word8]
  encode_time' [_, _, _, _, _] 0 = Left "Event timing overflow."
  encode_time' encoded 0 = Right (set_highest_bits encoded)
  encode_time' encoded raw = encode_time' (fromIntegral (mod raw 128) : encoded) (div raw 128)
  encode_track :: [Word8] -> Err [Word8]
  encode_track events =
    let
      track_with_end = events ++ [0, 255, 47, 0]
    in
      (\encoded_length -> [77, 84, 114, 107] ++ encoded_length ++ track_with_end) <$> bytes 4 "A track is too long" (fromIntegral (Prelude.length track_with_end))
  encode_tracks :: [Track] -> Err [Word8]
  encode_tracks tracks =
    case tracks of
      [] -> Right []
      Track channel header events : tracks' -> encode_events channel events >>= \x -> (++) (header ++ x) <$> encode_tracks tracks'
  event_number :: Event -> Word8
  event_number event =
    case event of
      End -> 128
      Start -> 144
  flat_map :: Monad f => (t -> f [u]) -> [t] -> f [u]
  flat_map = (.)((<$>) Prelude.concat) . mapM
  ins :: Word32 -> Note -> [Event'] -> [Event']
  ins time instrument track = let
    end_as_planned = Event' time End instrument in
    case track of
      [] -> [end_as_planned]
      Event' next_time event next_instrument : tl ->
        if next_time < time then
          if next_instrument == instrument then
            Event' next_time End instrument : Event' 0 Start instrument : tl
          else
            Event' next_time event next_instrument : ins (time - next_time) instrument tl
        else
          end_as_planned : Event' (next_time - time) event next_instrument : tl
  melodic_instrument :: Melodic_instrument -> Word8
  melodic_instrument instrument =
    case instrument of
      Bells -> 14
      Cello -> 42
      Contrabass -> 43
      Flute -> 73
      Guitar -> 24
      Harp -> 46
      Harpsichord -> 6
      Piano -> 0
      Strings -> 45
      Viola -> 41
      Violin -> 40
      Voice -> 53
  midi :: String -> Composition -> IO ()
  midi file_name composition = case encode composition of
    Left message -> Prelude.putStrLn("Error. " ++ message)
    Right encoded -> Data.ByteString.writeFile file_name (pack encoded)
  percussion_instrument :: Percussion_instrument -> Word8
  percussion_instrument instrument =
    case instrument of
      Bass_drum_1 -> 36
      Bass_drum_2 -> 35
      Cabasa -> 69
      Chinese_cymbal -> 52
      Claves -> 75
      Closed_hi_hat -> 42
      Cowbell -> 56
      Crash_cymbal_1 -> 49
      Crash_cymbal_2 -> 57
      Hand_clap -> 39
      High_agogo -> 67
      High_bongo -> 60
      High_timbale -> 65
      High_tom_1 -> 50
      High_tom_2 -> 48
      High_wood_block -> 76
      Long_guiro -> 74
      Long_whistle -> 72
      Low_agogo -> 68
      Low_bongo -> 61
      Low_conga -> 64
      Low_timbale -> 66
      Low_tom_1 -> 43
      Low_tom_2 -> 41
      Low_wood_block -> 77
      Maracas -> 70
      Mid_tom_1 -> 47
      Mid_tom_2 -> 45
      Mute_cuica -> 78
      Mute_high_conga -> 62
      Mute_triangle -> 80
      Open_cuica -> 79
      Open_hi_hat -> 46
      Open_high_conga -> 63
      Open_triangle -> 81
      Pedal_hi_hat -> 44
      Ride_bell -> 53
      Ride_cymbal_1 -> 51
      Ride_cymbal_2 -> 59
      Short_guiro -> 73
      Short_whistle -> 71
      Side_stick -> 37
      Snare_drum_1 -> 38
      Snare_drum_2 -> 40
      Splash_cymbal -> 55
      Tambourine -> 54
      Vibra_slap -> 58
  set_highest_bits :: [Word8] -> [Word8]
  set_highest_bits [] = undefined
  set_highest_bits [h] = [h]
  set_highest_bits (h : next : t) = h + 128 : set_highest_bits(next : t)
  start :: Percussion_event -> Event'
  start (Percussion_event time instrument) = Event' time Start (percussion_instrument instrument)
-----------------------------------------------------------------------------------------------------------------------------