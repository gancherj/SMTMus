module Mus.Solve where

import Data.SBV
import Data.SBV.Control
import Control.Monad

type Pitch = SInteger
pitchClass :: Pitch -> Pitch
pitchClass x = x `sMod` 8

validPitch x = x .>= 1

isStepBy :: Pitch -> Pitch -> Pitch -> SBool
isStepBy z x y =  (xm .== ympz) ||| (xm .== ymmz) 
    where
        xm = x --`sMod` 8
        ympz = (y + z) --`sMod` 8
        ymmz = (y - z) --`sMod` 8


isStep = isStepBy 1
isStepUp x y = (isStep x y) &&& y .> x
isStepDown x y = (isStep x y) &&& y .< x
isThird = isStepBy 2
isThirdUp x y = (isThird x y) &&& y .> x
isThirdDown x y = (isThird x y) &&& y .< x
isFourth = isStepBy 3
isFourthUp x y = (isFourth x y) &&& y .> x
isFourthDown x y = (isFourth x y) &&& y .< x
isFifth = isStepBy 4
isFifthUp x y = (isFifth x y) &&& y .> x
isFifthDown x y = (isFifth x y) &&& y .< x
isSixth = isStepBy 5
isSixthUp x y = (isSixth x y) &&& y .> x
isSixthDown x y = (isSixth x y) &&& y .< x
isSeventh = isStepBy 6
isSeventhUp x y = (isSeventh x y) &&& y .> x
isSeventhDown x y = (isSeventh x y) &&& y .< x

isLeap x y = bnot (isStep x y)
isLeapUp x y = (isLeap x y) &&& y .> x
isLeapDown x y = (isLeap x y) &&& y .< x

sAbs x = ite (x .< 0) (-x) x

largestLeap :: Int -> [SInteger] -> SInteger
largestLeap mlen melody = 
    let leaps = map (\i -> sAbs $ (melody !! i) - (melody !! (i + 1))) [0..(mlen - 2)] in
    foldl (\acc x -> smax acc x) (head leaps) leaps


validMelody :: Int -> [SInteger] -> Symbolic ()
validMelody mlen melody = do
    forM melody (\i -> constrain $ validPitch i)
    constrain $ (melody !! 0) .== 1
    constrain $ (melody !! (mlen - 1)) `sMod` 8 .== 0
    constrain $ (largestLeap mlen melody) .< 6
    forM_ [1..(mlen - 2)] (\i -> do
        let prev = melody !! (i - 1)
        let cur = melody !! i
            next = melody !! (i + 1)
        constrain $ isLeapUp prev cur ==> isStepDown cur next
        constrain $ isLeapDown prev cur ==> isStepUp cur next
        constrain $ cur ./= next
        constrain $ cur ./= prev
                          )

validSoprano :: Int -> [SInteger] -> Symbolic ()
validSoprano mlen melody = do
    constrain $ (melody !! (mlen - 2)) `sMod` 8 .== 7

validBass :: Int -> [SInteger] -> Symbolic ()
validBass mlen melody = do
    constrain $ (melody !! (mlen - 2)) `sMod` 8 .== 4


perfectHarmony :: SInteger -> SInteger -> SBool
perfectHarmony x y = (x .== y) ||| (isFourth x y) ||| (isFifth x y)

parallelMotionCheck :: SInteger -> SInteger -> SInteger -> SInteger -> SBool
parallelMotionCheck s sn b bn =
    bnot $ bOr [
        bAnd [s .== 2, sn .== 7, b .== 3, bn .== 3]
    ]

validHarmony :: Int -> [SInteger] -> [SInteger] -> Symbolic ()
validHarmony mlen sop bass = 
    forM_ [0..(mlen - 1)] $ \i -> do
        let sn = (sop !! i)
            bn = (bass !! i)
        constrain $ bOr $ [
            sn .== bn,
            isThird bn sn,
            isFourthDown bn sn,
            isFifthUp bn sn,
            isSixth bn sn]
        if i < mlen - 1 then 
            do
                let snext = (sop !! (i + 1))
                    bnext = (bass !! (i + 1))
                constrain $ perfectHarmony sn bn ==> bnot (perfectHarmony snext bnext)
                constrain $ isLeap sn snext ==> bnot (isLeap bn bnext)
                constrain $ isLeap bn bnext ==> bnot (isLeap sn snext)
                constrain $ parallelMotionCheck sn snext bn bnext
        else return ()


showCpt :: [Integer] -> [Integer] -> IO ()
showCpt s b = do
    putStrLn ""
    putStrLn $ show s
    putStrLn $ show b
    putStrLn ""


genMelody :: Int -> Symbolic [([Integer], [Integer])]
genMelody bound = do
    let melodylen = 8
    melodySop <- mkFreeVars melodylen
    melodyBass <- mkFreeVars melodylen
    validMelody melodylen melodySop
    validMelody melodylen melodyBass
    validHarmony melodylen melodySop melodyBass
    --validSoprano melodylen melodySop
    --validBass melodylen melodyBass

    let next i sofar = if i > bound then return sofar else
            do
                cs <- checkSat
                case cs of
                  Unsat -> return sofar
                  Unk -> return sofar
                  Sat -> do
                      vMelodySop <- forM melodySop getValue
                      vMelodyBass <- forM melodyBass getValue
                      io $ showCpt vMelodySop vMelodyBass
                      let diseqs = map (\i -> (melodySop !! i) ./= literal (vMelodySop !! i)) [0..(melodylen-1)]
                      let diseqs2 = map (\i -> (melodyBass !! i) ./= literal (vMelodyBass !! i)) [0..(melodylen-1)]
                      constrain $ bOr $ diseqs ++ diseqs2
                      next (i + 1) ((vMelodyBass, vMelodySop) : sofar)
    query $ next 0 []



