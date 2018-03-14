-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Midi where
  import Data.Bifunctor
  import Data.Bits
  import Data.ByteString
  import Data.Int
  import Data.List
  import Data.Map
  import Data.Word
  data Composition = Composition Word8 Word8 [Word8] [Melody] [Percussion_event] deriving Show
  type Err t = Either String t
  data Event = Event Word8 [Int8] [Int8] deriving Show
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
    Timpani |
    Viola |
    Violin |
    Voice
      deriving Show
  data Melody = Melody Melodic_instrument [Event] deriving Show
  data Percussion_event = Percussion_event Word8 [Percussion_instrument] deriving Show
  data Percussion_instrument =
    Bass_drum_1 |
    Bass_drum_2 |
    Claves |
    Cowbell |
    Hand_clap |
    High_agogo |
    High_bongo |
    High_timbale |
    High_tom_1 |
    High_tom_2 |
    High_wood_block |
    Low_agogo |
    Low_bongo |
    Low_conga |
    Low_timbale |
    Low_tom_1 |
    Low_tom_2 |
    Low_wood_block |
    Mute_high_conga |
    Mute_triangle |
    Open_high_conga |
    Open_triangle |
    Short_whistle |
    Side_stick |
    Tambourine |
    Vibra_slap
      deriving (Eq, Show)
  data Track = Track Word8 [Word8] [Event] deriving Show
  data Velocities = Velocities Word8 (Map Word8 Word8) deriving Show
  bytes :: Word8 -> Word32 -> [Word8]
  bytes = bytes' []
  bytes' :: [Word8] -> Word8 -> Word32 -> [Word8]
  bytes' x n y =
    case n of
      0 -> x
      _ -> bytes' (fromIntegral (mod y 256) : x) (n - 1) (div y 256)
  encode :: Composition -> Err [Word8]
  encode (Composition breakdown tempo velocities melody percussion) =
    if breakdown < 8
      then
        if tempo < 4
          then Left "Tempo should be at least 4 beats per minute."
          else
            case velocities of
              [] -> Left "The list of velocities should not be empty."
              velocity : velocities' ->
                (
                  velocity_map (Velocities 1 (Data.Map.singleton 0 velocity)) velocities' >>=
                  \velocities'' ->
                    (
                      encode_melody (Data.List.delete 9 [0 .. 15]) melody >>=
                      \(tracks, melody') ->
                        (
                          (++) [77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 0, tracks, 0, shiftL 1 (fromIntegral breakdown)] <$>
                          encode_tracks
                            velocities''
                            (
                              Track
                                9
                                ([0, 255, 81, 3] ++ bytes 3 (div 60000000 (fromIntegral tempo)))
                                (encode_percussion [] percussion) :
                              melody'))))
      else Left "Quarternote breakdown should be no more than 1 / 2 ^ 7."
  encode_event :: Velocities -> Word8 -> Word8 -> Event -> Err (Word8, [Word8])
  encode_event (Velocities bar_length velocities) channel time (Event time' end start) =
    let
      time'' = mod (time + time') bar_length
      encode_my_event = encode_event' channel (unsafe_lookup velocities time'')
    in
      (
        (,) time'' <$> 
        (case end of
          [] ->
            case start of
              [] -> Left "Empty event."
              note : start' ->
                encode_my_event time' 144 note >>= \note' -> (++) note' <$> map_and_cat (encode_my_event 0 144) start'
          note : end' ->
            (
              encode_my_event time' 128 note >>=
              \note' ->
                (
                  map_and_cat (encode_my_event 0 128) end' >>=
                  \end'' -> (++) (note' ++ end'') <$> map_and_cat (encode_my_event 0 144) start))))
  encode_event' :: Word8 -> Word8 -> Word8 -> Word8 -> Int8 -> Err [Word8]
  encode_event' channel velocity time event note =
    if note < 0
      then Left "Notes should not be negative."
      else Right (encode_time time ++ [event + channel, fromIntegral note, velocity])
  encode_events :: Velocities -> Word8 -> Word8 -> [Event] -> Err [Word8]
  encode_events velocities channel time events =
    case events of
      [] -> Right []
      event : events' ->
        (
          encode_event velocities channel time event >>=
          \(time', event') -> (++) event' <$> encode_events velocities channel time' events')
  encode_melody :: [Word8] -> [Melody] -> Err (Word8, [Track])
  encode_melody channels melodies =
    case melodies of
      [] -> Right (1, [])
      Melody instrument melody : melodies' ->
        case channels of
          [] -> Left "The number of melody tracks is limited to 15."
          channel : channels' ->
            (
              bimap
                ((+) 1)
                ((:)
                  (Track
                    channel
                    [0, 192 + channel, melodic_instrument instrument]
                    melody)) <$>
              encode_melody channels' melodies')
  encode_percussion :: [Int8] -> [Percussion_event] -> [Event]
  encode_percussion end percussion =
    case percussion of
      [] ->
        case end of
          [] -> []
          _ -> [Event 5 end []]
      Percussion_event time instruments : percussion' ->
        let
          instruments' = percussion_instrument <$> instruments
        in
          Event time end instruments' : encode_percussion instruments' percussion'
  encode_time :: Word8 -> [Word8]
  encode_time time = if time < 128 then [time] else [129, time - 128]
  encode_track :: Velocities -> Track -> Err [Word8]
  encode_track velocities (Track channel header events) =
    (
      (\events' ->
        let
          track = header ++ events' ++ [0, 255, 47, 0]
        in
          [77, 84, 114, 107] ++ bytes 4 (fromIntegral (Prelude.length track)) ++ track) <$>
      encode_events velocities channel 0 events)
  encode_tracks :: Velocities -> [Track] -> Err [Word8]
  encode_tracks velocities = map_and_cat (encode_track velocities)
  map_and_cat :: Monad f => (t -> f [u]) -> [t] -> f [u]
  map_and_cat f x = Prelude.concat <$> mapM f x
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
      Timpani -> 47
      Viola -> 41
      Violin -> 40
      Voice -> 53
  midi :: String -> Composition -> IO ()
  midi file composition =
    case encode composition of
      Left err -> Prelude.putStrLn err
      Right encoding -> Data.ByteString.writeFile (file ++ ".mid") (pack encoding)
  percussion_instrument :: Percussion_instrument -> Int8
  percussion_instrument instrument =
    case instrument of
      Bass_drum_1 -> 36
      Bass_drum_2 -> 35
      Claves -> 75
      Cowbell -> 56
      Hand_clap -> 39
      High_agogo -> 67
      High_bongo -> 60
      High_timbale -> 65
      High_tom_1 -> 50
      High_tom_2 -> 48
      High_wood_block -> 76
      Low_agogo -> 68
      Low_bongo -> 61
      Low_conga -> 64
      Low_timbale -> 66
      Low_tom_1 -> 43
      Low_tom_2 -> 41
      Low_wood_block -> 77
      Mute_high_conga -> 62
      Mute_triangle -> 80
      Open_high_conga -> 63
      Open_triangle -> 81
      Short_whistle -> 71
      Side_stick -> 37
      Tambourine -> 54
      Vibra_slap -> 58
  unsafe_lookup :: Ord t => Map t u -> t -> u
  unsafe_lookup m x =
    case Data.Map.lookup x m of
      Just y -> y
      Nothing -> undefined
  velocity_map :: Velocities -> [Word8] -> Err Velocities
  velocity_map velocities @ (Velocities time velocities') velocities_0 =
    case time of
      0 -> Left "The list of velocities is too long."
      _ ->
        case velocities_0 of
          [] -> Right velocities
          velocity : velocities_1 ->
            if velocity < 128
              then velocity_map (Velocities (time + 1) (Data.Map.insert time velocity velocities')) velocities_1
              else Left "Velocities should be in range {0, ..., 127}."
-----------------------------------------------------------------------------------------------------------------------------