-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Midi where
  import Data.Bifunctor
  import Data.Bits
  import Data.ByteString
  import Data.Word
  data Composition = Composition Word8 Word8 [Melody_track] [Percussion_event] deriving Show
  type Err t = Either String t
  data Event = End | Start deriving Show
  data Event' = Event' Word8 Event Word8 deriving Show
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
  data Melody_track = Melody_track Melodic_instrument [Event'] deriving Show
  data Percussion_event = Percussion_event Word8 Percussion_instrument deriving Show
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
  data Track = Track Word8 [Word8] [Event'] deriving Show
  bytes :: Word8 -> Word32 -> [Word8]
  bytes = bytes' []
  bytes' :: [Word8] -> Word8 -> Word32 -> [Word8]
  bytes' x n y =
    case n of
      0 -> x
      _ -> bytes' (fromIntegral (mod y 256) : x) (n - 1) (div y 256)
  encode :: Composition -> Err [Word8]
  encode (Composition breakdown tempo melody percussion) =
    if breakdown < 8
      then
        if tempo < 4
          then Left "Tempo should be at least 4 beats per minute."
          else
            (
              encode_melody [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15] melody >>=
              \(tracks, melody') ->
                (
                  (++) [77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 0, tracks, 0, shiftL 1 (fromIntegral breakdown)] <$>
                  encode_tracks
                    (
                      Track
                        9
                        ([0, 255, 81, 3] ++ bytes 3 (div 60000000 (fromIntegral tempo)))
                        (encode_percussion (start <$> percussion)) :
                      melody')))
      else Left "Quarternote breakdown should be no more than 1 / 2 ^ 7."
  encode_event :: Word8 -> Event' -> Err [Word8]
  encode_event channel (Event' time event note) =
    if note < 128
      then Right (encode_time time ++ [event_number event + channel, note, 127])
      else Left "The range of notes is {0,...,127}."
  encode_events :: Word8 -> [Event'] -> Err [Word8]
  encode_events channel = map_and_cat (encode_event channel)
  encode_melody :: [Word8] -> [Melody_track] -> Err (Word8, [Track])
  encode_melody channels melodies =
    case melodies of
      [] -> Right (1, [])
      Melody_track instrument melody : melodies' ->
        case channels of
          [] -> Left "The number of melody tracks is limited to 15."
          channel : channels' ->
            (
              bimap ((+) 1) ((:) (Track channel [0, 192 + channel, melodic_instrument instrument] melody)) <$>
              encode_melody channels' melodies')
  encode_percussion :: [Event'] -> [Event']
  encode_percussion percussion =
    case percussion of
      [] -> []
      event' @ (Event' _ event instrument) : percussion' ->
        (
          event' :
          encode_percussion
            (case event of
              End -> percussion'
              Start -> insert_end 4 instrument percussion'))
  encode_time :: Word8 -> [Word8]
  encode_time time = if time < 128 then [time] else [129, time - 128]
  encode_track :: Track -> Err [Word8]
  encode_track (Track channel header events) =
    (
      (\events' ->
        let
          track = header ++ events' ++ [0, 255, 47, 0]
        in
          [77, 84, 114, 107] ++ bytes 4 (fromIntegral (Prelude.length track)) ++ track) <$>
      encode_events channel events)
  encode_tracks :: [Track] -> Err [Word8]
  encode_tracks = map_and_cat encode_track
  event_number :: Event -> Word8
  event_number event =
    case event of
      End -> 128
      Start -> 144
  insert_end :: Word8 -> Word8 -> [Event'] -> [Event']
  insert_end time instrument track =
    let
      end_as_planned = Event' time End instrument
    in
      case track of
        [] -> [end_as_planned]
        event' @ (Event' time' event instrument') : track' ->
          if time' < time
            then
              if instrument' == instrument
                then
                  case event of
                    End -> track
                    Start -> Event' time' End instrument : Event' 0 Start instrument : track'
                else event' : insert_end (time - time') instrument track'
            else end_as_planned : Event' (time' - time) event instrument' : track'
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
  percussion_instrument :: Percussion_instrument -> Word8
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
  start :: Percussion_event -> Event'
  start (Percussion_event time instrument) = Event' time Start (percussion_instrument instrument)
-----------------------------------------------------------------------------------------------------------------------------