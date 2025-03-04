module Eval where
import Val
import Utils (trim, stripQuotes)


-- Main evaluation function for operators and built-in FORTH functions
eval :: String -> [Val] -> [Val]

-- Arithmetic Operators

-- Multiplication
eval "*" (Integer x : Integer y : tl) = Integer (x * y) : tl
eval "*" (x : y : tl) = Real (toFloat x * toFloat y) : tl
eval "*" _ = error "Stack underflow"

-- Addition
eval "+" (Integer x : Integer y : tl) = Integer (x + y) : tl
eval "+" (x : y : tl) = Real (toFloat x + toFloat y) : tl
eval "+" _ = error "Stack underflow"

-- Subtraction
eval "-" (Integer x : Integer y : tl) = Integer (y - x) : tl
eval "-" (x : y : tl) = Real (toFloat y - toFloat x) : tl
eval "-" _ = error "Stack underflow"

-- Division
eval "/" (Integer x : Integer y : tl)
    | x == 0    = error "Division by zero"
    | otherwise = Integer (y `div` x) : tl
eval "/" (x : y : tl)
    | toFloat x == 0 = error "Division by zero"
    | otherwise      = Real (toFloat y / toFloat x) : tl
eval "/" _ = error "Stack underflow"

-- Power (^)
eval "^" (Integer x : Integer y : tl) = Integer (y ^ x) : tl
eval "^" (x : y : tl) = Real ((toFloat y) ** (toFloat x)) : tl
eval "^" _ = error "Stack underflow"

-- String and Formatting Functions

-- EMIT: Print ASCII character corresponding to the number on top of the stack
eval "EMIT" (Integer x : tl)
    | x >= 0 && x <= 255 = Id [toEnum $ fromIntegral x] : tl
    | otherwise          = error "Invalid argument for EMIT: must be an integer between 0 and 255"
eval "EMIT" [] = error "Stack underflow for EMIT"
eval "EMIT" _ = error "Invalid argument for EMIT: must be an integer"

-- CR: Print a newline
eval "CR" stack = Id "\n" : stack

-- STR: Convert top element of the stack to a string
eval "STR" (Integer x : tl) = Id (show x) : tl
eval "STR" (Real x : tl)    = Id (show x) : tl
eval "STR" _                = error "Stack underflow"



-- CONCAT2: Concatenate two strings from the stack
eval "CONCAT2" (Id s1 : Id s2 : tl) = Id (unwords [stripQuotes s2, stripQuotes s1]) : tl
eval "CONCAT2" (_ : _ : _)          = error "Invalid arguments for CONCAT2: both must be strings"
eval "CONCAT2" _                    = error "Stack underflow for CONCAT2"

-- CONCAT3: Concatenate three strings from the stack with proper spacing
eval "CONCAT3" (Id s1 : Id s2 : Id s3 : tl) = Id (unwords [stripQuotes s3, stripQuotes s2, stripQuotes s1]) : tl
eval "CONCAT3" (_ : _ : _ : _) = error "Invalid arguments for CONCAT3: all must be strings"
eval "CONCAT3" _               = error "Stack underflow for CONCAT3"





-- Duplicate the element at the top of the stack
eval "DUP" (x : tl) = x : x : tl
eval "DUP" []       = error "Stack underflow"

-- This must be the last rule:
-- It assumes no match is made and preserves the string as an argument.
eval s l = Id s : l


-- Variant of eval with output

-- Print element at the top of the stack.
evalOut "." (Id x : tl, out)     = (tl, out ++ x)
evalOut "." (Integer i : tl, out) = (tl, out ++ show i)
evalOut "." (Real x : tl, out)    = (tl, out ++ show x)
evalOut "." ([] , _)              = error "Stack underflow"

-- This has to be the last case:
-- If no special case, ask eval to deal with it and propagate output.
evalOut op (stack, out) = (eval op stack, out)
