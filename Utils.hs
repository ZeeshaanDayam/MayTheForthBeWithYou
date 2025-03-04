-- Utils.hs
module Utils (trim, stripQuotes) where

-- Helper function to trim leading and trailing spaces
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Helper function to strip surrounding quotes and trim the result
stripQuotes :: String -> String
stripQuotes s =
  let s' = trim s
  in if length s' >= 2 && head s' == '"' && last s' == '"'
        then trim (init (tail s'))
        else s'
