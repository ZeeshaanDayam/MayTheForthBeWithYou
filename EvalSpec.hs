-- HSpec tests for Eval.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do

    -- Tests for "*"
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    -- Tests for "+"
    context "+" $ do
        it "adds integers" $ do
            eval "+" [Integer 1, Integer 2] `shouldBe` [Integer 3]

        it "adds floats" $ do
            eval "+" [Real 1.5, Integer 2] `shouldBe` [Real 3.5]

        it "errors on too few arguments" $ do
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"

    -- Tests for "-"
    context "-" $ do
        it "subtracts integers" $ do
            eval "-" [Integer 1, Integer 5] `shouldBe` [Integer 4]

        it "subtracts floats" $ do
            eval "-" [Real 1.5, Real 5.0] `shouldBe` [Real 3.5]

        it "errors on too few arguments" $ do
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"

    -- Tests for "/"
    context "/" $ do
        it "divides integers" $ do
            eval "/" [Integer 2, Integer 8] `shouldBe` [Integer 4]

        it "divides floats" $ do
            eval "/" [Real 2.0, Real 8.0] `shouldBe` [Real 4.0]

        it "errors on division by zero" $ do
            evaluate (eval "/" [Integer 0, Integer 8]) `shouldThrow` errorCall "Division by zero"

    -- Tests for "^"
    context "^" $ do
        it "computes power of integers" $ do
            eval "^" [Integer 2, Integer 3] `shouldBe` [Integer 9]

        it "computes power of floats" $ do
            eval "^" [Real 2.0, Real 3.0] `shouldBe` [Real 9.0]

    -- Tests for EMIT
    context "EMIT" $ do
        it "emits ASCII characters from numbers" $ do
            eval "EMIT" [Integer 65] `shouldBe` [Id "A"]

        it "errors on stack underflow" $ do
            evaluate (eval "EMIT" []) `shouldThrow` errorCall "Stack underflow for EMIT"

        it "errors on invalid argument (non-integer)" $ do
            evaluate (eval "EMIT" [Real 65.5]) `shouldThrow` errorCall "Invalid argument for EMIT: must be an integer"
            evaluate (eval "EMIT" [Id "A"]) `shouldThrow` errorCall "Invalid argument for EMIT: must be an integer"

        it "errors on invalid argument (out of range)" $ do
            evaluate (eval "EMIT" [Integer (-1)]) `shouldThrow` errorCall "Invalid argument for EMIT: must be an integer between 0 and 255"
            evaluate (eval "EMIT" [Integer 300]) `shouldThrow` errorCall "Invalid argument for EMIT: must be an integer between 0 and 255"


    -- Tests for CR
    context "CR" $ do
        it "adds a newline to the stack" $ do
            eval "CR" [] `shouldBe` [Id "\n"]

    -- Tests for STR
    context "STR" $ do
        it "converts top element to string" $ do
            eval "STR" [Integer 123] `shouldBe` [Id "123"]
            eval "STR" [Real 4.56] `shouldBe` [Id "4.56"]

        it "errors on empty stack" $ do
            evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"

    -- Tests for CONCAT2
    context "CONCAT2" $ do
        it "concatenates two strings from the stack" $ do
            eval "CONCAT2" [Id "World", Id "Hello "] `shouldBe` [Id "Hello World"]

        it "errors on stack underflow" $ do
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack underflow for CONCAT2"
            evaluate (eval "CONCAT2" [Id "Hello"]) `shouldThrow` errorCall "Stack underflow for CONCAT2"

        it "errors on invalid arguments" $ do
            evaluate (eval "CONCAT2" [Integer 123, Id "Hello"]) `shouldThrow` errorCall "Invalid arguments for CONCAT2: both must be strings"

    -- Tests for CONCAT3
    context "CONCAT3" $ do -- FIXED INDENTATION HERE!
        it "concatenates three strings from the stack without extra spaces" $ do
            eval "CONCAT3" [Id "! ", Id "World", Id "Hello "] 
                `shouldBe` [Id "Hello World !"]

        it "errors on stack underflow" $ do
            evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Stack underflow for CONCAT3"
            evaluate (eval "CONCAT3" [Id "!"]) `shouldThrow` errorCall "Stack underflow for CONCAT3"
            evaluate (eval "CONCAT3" [Id "! ", Id "World"]) 
                `shouldThrow` errorCall "Stack underflow for CONCAT3"

        it "errors on invalid arguments" $ do
            evaluate (eval "CONCAT3" [Integer 123, Id "! ", Id "!"]) 
                `shouldThrow` errorCall "Invalid arguments for CONCAT3: all must be strings"

-- Tests for evalOut remain unchanged since they already cover output behavior.
