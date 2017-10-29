module Mus.Play where
import Haskore

intToNote oct i = case i of
                1 -> (C, oct)
                2 -> (D, oct)
                3 -> (E, oct)
                4 -> (F, oct)
                5 -> (G, oct)
                6 -> (A, oct)
                7 -> (B, oct)
                8 -> (C, oct + 1)
                9 -> (D, oct + 1)
                10 -> (E, oct + 1)
                11 -> (F, oct + 1)
                12 -> (G, oct + 1)
                13 -> (A, oct + 1)
                14 -> (B, oct + 1)



arrToHaskore :: ([Integer], [Integer]) -> Music
arrToHaskore (b,s) =
    let b_notes = map (\i -> Note (intToNote 4 i) 1 []) b
        s_notes = map (\i -> Note (intToNote 5 i) 1 []) s
        b_mel = foldl (\acc n -> acc :+: n) (head b_notes) (tail b_notes)
        s_mel = foldl (\acc n -> acc :+: n) (head s_notes) (tail s_notes) in
    Tempo 3 (b_mel :=: s_mel)

makeMidi :: String -> ([Integer], [Integer]) -> IO ()
makeMidi n (b,s) = do
    let mus = arrToHaskore (b,s) 
        midi = testMidi mus
    outputMidiFile n midi


