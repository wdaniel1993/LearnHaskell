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
parseOperation [a] = lookup a dispatch
parseOperation _ = Nothing

parseExp ::[String] -> [Expression]
parseExp [] = []
parseExp (x:xs)
  |isNum x = buildExp $ Number $ fromJust $ parseDouble x
  |isOperation x = buildExp $ Operation $ fromJust $ parseOperation x
  |otherwise = parseExp xs
  where buildExp h = h:parseExp xs

calculateRPN ::[Expression] -> Double
calculateRPN e = head $ foldl foldingMethod [] e
    where foldingMethod stack (Number d) = d:stack
          foldingMethod (x:y:xs) (Operation o) = (o y x):xs

main = do
  line <- getLine
  unless (null line) $
    do
      let
        w = words line
        e = parseExp w
      putStrLn $ show $ calculateRPN e
