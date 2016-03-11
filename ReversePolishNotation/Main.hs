import System.IO
import Control.Monad
import Data.Maybe

data Expression = Number Double | Operation (Double  -> Double  -> Double )

dispatch :: [(Char, Double  -> Double  -> Double )]
dispatch =  [ ('+', (+))
            , ('-', (-))
            , ('*', (*))
            , ('/', (/))
            ]

isNum:: String -> Bool
isNum = isParsable parseDouble

isOperation:: String -> Bool
isOperation = isParsable parseOperation

isParsable :: (String -> Maybe a) -> String -> Bool
isParsable f str = isJust $ f str

parseDouble:: String -> Maybe Double
parseDouble str =
    case reads str :: [(Double, String)] of
      [(a, "")] -> Just a
      _         -> Nothing

parseOperation:: String -> Maybe (Double  -> Double  -> Double )
parseOperation str =
    case reads str :: [(Char, String)] of
      [(a, "")] -> lookup a dispatch
      _         -> Nothing

parseExp ::[String] -> [Expression]
parseExp [] = []
parseExp (x:xs)
  |isNum x = buildExp $ Number $ fromJust $ parseDouble x
  |isOperation x = buildExp $ Operation $ fromJust $ parseOperation x
  |otherwise = parseExp xs
  where buildExp h = h:parseExp xs

calculateRPN ::[Expression] -> Double
calculateRPN e = head $ foldl (\stack ex -> case ex of
  Number d -> d:stack
  Operation o -> (foldr1 o $ take 2 stack):(tail $ tail stack)) [] e

main = do
  line <- getLine
  unless (null line) $
    do
      let
        w = words line
        exps = parseExp w
      putStrLn $ show $ calculateRPN exps
