-----------------------------------------------------------------------------------------------------------------------------
module Midi where
  import Data.ByteString hiding(concat, length, putStrLn, zip)
  import Data.Word
  import Prelude hiding(writeFile)
  data Event = End | Start
  data Melodic_instrument =
    Bells |
    Cello |
    Contrabass |
    Flute |
    Guitar |
    Harpsichord |
    Piano |
    Pizzicato |
    Viola |
    Violin |
    Voice
  data Percussion_instrument =
    Acoustic_bass_drum |
    Bass_drum_1 |
    Claves |
    Hand_clap |
    Hi_bongo |
    Hi_wood_block |
    High_tom |
    Low_bongo |
    Low_conga |
    Low_tom |
    Low_wood_block |
    Maracas |
    Mute_hi_conga |
    Mute_triangle |
    Open_hi_conga |
    Open_triangle |
    Side_stick
    deriving(Eq)
  encode ::
    (Word32, [(Melodic_instrument, [(Word32, Event, Word8)])], [(Word32, Percussion_instrument)]) -> Either String [Word8]
  encode(tempo, melody, percussion) = let
    tracks = fromIntegral(length melody) in
    if tracks > 15 then Left "Too many melody tracks." else case hex 3 tempo of
      Just encoded_tempo -> case encode_percussion((\(time, instrument) -> (time, Start, instrument)) <$> percussion) of
        Just encoded_percussion ->
          case mapM encode_melody (zip [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15] melody) of
            Left message -> Left message
            Right encoded_melody ->
              ([77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 0, tracks + 1, 0, 8] ++) <$>
              flat_map encode_track (([0, 255, 81, 3] ++ encoded_tempo ++ encoded_percussion) : encoded_melody)
        Nothing -> Left "Event timing overflow."
      Nothing -> Left "Tempo too slow."
  encode_event :: Word8 -> (Word32, Event, Word8) -> Either String [Word8]
  encode_event channel (time, event, note) =
    --if note < 128 then (++ [event_number event + channel, note, 127]) <$> encode_time time else Left "A note is too high."
    if note < 128 then case encode_time time of
      Just _ -> undefined
      Nothing -> Left "Event timing overflow." else Left "A note is too high."
  encode_melody :: (Word8, (Melodic_instrument, [(Word32, Event, Word8)])) -> Either String [Word8]
  encode_melody(channel, (instrument, events)) =
    ([0, 192 + channel, melodic_instrument_number(instrument)] ++) <$> flat_map(encode_event channel) events
  encode_percussion :: [(Word32, Event, Percussion_instrument)] -> Maybe [Word8]
  encode_percussion [] = Just []
  encode_percussion((time, event, instrument) : tail) = case encode_time time of
    Just encoded_time ->
      ((encoded_time ++ [event_number event + 9, percussion_instrument_number instrument, 127]) ++) <$>
      encode_percussion(case event of
        End -> tail
        Start -> ins instrument 32 tail)
    Nothing -> Nothing
  encode_time :: Word32 -> Maybe [Word8]
  encode_time 0 = Just [0]
  encode_time time = encode_time' [] time
  encode_time' :: [Word8] -> Word32 -> Maybe [Word8]
  encode_time' [_, _, _, _, _] 0 = Nothing
  encode_time' encoded 0 = Just(set_highest_bits encoded)
  encode_time' encoded raw = encode_time'(fromIntegral(mod raw 128) : encoded)(div raw 128)
  encode_track :: [Word8] -> Either String [Word8]
  encode_track events = let
    track_with_end = events ++ [0, 255, 47, 0] in
    case hex 4 (fromIntegral(length track_with_end)) of
      Just encoded_length -> Right([77, 84, 114, 107] ++ encoded_length ++ track_with_end)
      Nothing -> Left "A track is too long."
  event_number :: Event -> Word8
  event_number End = 128
  event_number Start = 144
  flat_map :: Monad f => (t -> f [u]) -> [t] -> f [u]
  flat_map = (.)((<$>) concat) . mapM
  hex :: Word8 -> Word32 -> Maybe [Word8]
  hex 0 0 = Just []
  hex 0 _ = Nothing
  hex len x = (++ [fromIntegral(mod x 256)]) <$> hex(len - 1)(div x 256)
  ins ::
    Percussion_instrument -> Word32 -> [(Word32, Event, Percussion_instrument)] -> [(Word32, Event, Percussion_instrument)]
  ins instrument time track = let
    end_as_planned = (time, End, instrument) in
    case track of
      [] -> [end_as_planned]
      (next_time, event, next_instrument) : tail ->
        if next_time < time then
          if next_instrument == instrument then
            (next_time, End, instrument) : (0, Start, instrument) : tail
          else
            (next_time, event, next_instrument) : ins instrument (time - next_time) tail
        else
          end_as_planned : (next_time - time, event, next_instrument) : tail
  melodic_instrument_number :: Melodic_instrument -> Word8
  melodic_instrument_number Bells = 14
  melodic_instrument_number Cello = 42
  melodic_instrument_number Contrabass = 43
  melodic_instrument_number Flute = 73
  melodic_instrument_number Guitar = 24
  melodic_instrument_number Harpsichord = 6
  melodic_instrument_number Piano = 0
  melodic_instrument_number Pizzicato = 45
  melodic_instrument_number Viola = 41
  melodic_instrument_number Violin = 40
  melodic_instrument_number Voice = 53
  midi :: String -> (Word32, [(Melodic_instrument, [(Word32, Event, Word8)])], [(Word32, Percussion_instrument)]) -> IO()
  midi file_name composition = case encode composition of
    Left message -> putStrLn("Error. " ++ message)
    Right encoded -> writeFile file_name (pack encoded)
  percussion_instrument_number :: Percussion_instrument -> Word8
  percussion_instrument_number Acoustic_bass_drum = 35
  percussion_instrument_number Bass_drum_1 = 36
  percussion_instrument_number Hand_clap = 39
  percussion_instrument_number Hi_bongo = 60
  percussion_instrument_number Hi_wood_block = 76
  percussion_instrument_number High_tom = 50
  percussion_instrument_number Low_bongo = 61
  percussion_instrument_number Low_conga = 64
  percussion_instrument_number Low_tom = 45
  percussion_instrument_number Low_wood_block = 77
  percussion_instrument_number Maracas = 70
  percussion_instrument_number Mute_hi_conga = 62
  percussion_instrument_number Mute_triangle = 80
  percussion_instrument_number Open_hi_conga = 63
  percussion_instrument_number Open_triangle = 81
  percussion_instrument_number Side_stick = 37
  set_highest_bits :: [Word8] -> [Word8]
  set_highest_bits [head] = [head]
  set_highest_bits(head : next : tail) = head + 128 : set_highest_bits(next : tail)