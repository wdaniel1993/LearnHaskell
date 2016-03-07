module Main where

  doubleMe x = x * 2

  getFourth numbers = numbers !! 3

  main :: IO ()
  main = print (doubleMe (getFourth [1,2,3,4,5]))
