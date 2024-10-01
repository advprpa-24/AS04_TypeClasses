{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Test.Hspec ( hspec, describe, it, shouldBe, shouldSatisfy, Spec )
import Lang.Imp
import qualified Map.TreeMap as M

evalSpec :: Spec
evalSpec = 
  describe "eval" $ do 
    it "eval (AConst 1) empty == 1" $
      eval (AConst 1) emptyState `shouldBe` (1 :: Integer)

execSpec :: Spec
execSpec = 
  describe "exec" $ do 
    it "exec CSkip emptyState == emptyState" $
      exec CSkip emptyState `shouldBe` emptyState
    it "M.toList (exec (CAssign \"a\" (AConst 2) emptyState) == [(\"a\",2)]" $
      M.toList (exec (CAssign "a" (AConst 2)) emptyState) `shouldBe` [("a",2)]



{-
Factorial:

z := x
y := 1
while (! (z = 0))
  y := y * z
  z := z - 1
-}
fact :: Cmd
fact = 
  CSeq [
    CAssign "z" (AVar "x"),
    CAssign "y" (AConst 1),
    CWhile (BNot (REq (AVar "z") (AConst 0))) (CSeq [
      CAssign "y" (AMul (AVar "y") (AVar "z")),
      CAssign "z" (AMinus (AVar "z") (AConst 1))
    ])
  ]

factSpec :: Spec
factSpec = describe "fact" $ do 
    -- fact [x=5] ~> y=120
    it "M.lookup \"y\" (exec fact (M.fromList [(\"x\", 5)])) == Just 120" $
      M.lookup "y" (exec fact (M.fromList [("x", 5)])) `shouldBe` Just 120 
    

{-
Quotient remainder:

q := 0
r := m
while (n < 2)
  q := q + 1
  r := r - n
-}
intDiv :: Cmd
intDiv = 
  CSeq [
    CAssign "q" (AConst 0),
    CAssign "r" (AVar "m"),
    CWhile (RLt (AVar "n") (AVar "r")) (CSeq [
        CAssign "q" (APlus (AVar "q") (AConst 1)),
        CAssign "r" (AMinus (AVar "r") (AVar "n"))
    ])
  ]

intDivSpec :: Spec
intDivSpec = describe "intDiv" $ do 
    -- intDiv 5 2 == q_uotient=2 r_emainder=1
    it "exec intDiv (M.fromList [(\"m\", 5), (\"n\", 2)]) ~> \"q\" -> 2, \"r\" -> 1" $
      exec intDiv (M.fromList [("m", 5), ("n", 2)]) `shouldSatisfy` 
        (\s -> M.lookup "q" s == Just 2 && M.lookup "r" s == Just 1)


{-
function gcd(a, b)
    while b ≠ 0
        t := b
        b := a mod b
        a := t
    return a
-}
gcd' :: Cmd
gcd' = CSeq [
  CWhile (BNot (REq (AVar "b") (AConst 0))) (CSeq [
        CAssign "t" (AVar "b"),
        CAssign "b" (AMod (AVar "a") (AVar "b")),
        CAssign "a" (AVar "t")
    ])
  ]


gcdSpec :: Spec
gcdSpec = describe "intDiv" $ do 
    -- intDiv 5 2 == q_uotient=2 r_emainder=1
    it "M.lookup \"a\" (exec gcd' (M.fromList [(\"a\", 252), (\"b\", 105)])) == Just 21" $
      M.lookup "a" (exec gcd' (M.fromList [("a", 252), ("b", 105)])) `shouldBe` Just 21 
    




main :: IO ()
main = hspec $ do
  describe "1. Evaluation" $ do
    evalSpec
  describe "2. Execution" $ do
    execSpec
  describe "3. Example programs" $ do
    factSpec
    intDivSpec
    gcdSpec
   
    