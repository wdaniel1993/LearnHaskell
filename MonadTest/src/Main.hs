module Main where

    type Birds = Int
    type Pole = (Birds,Birds)

    landLeft :: Birds -> Pole -> Maybe Pole
    landLeft n (left,right)
        | abs ((left + n) - right) < 4 = Just (left + n, right)
        | otherwise                    = Nothing

    landRight :: Birds -> Pole -> Maybe Pole
    landRight n (left,right)
        | abs (left - (right + n)) < 4 = Just (left, right + n)
        | otherwise                    = Nothing

    routine :: Maybe Pole
    routine = do
        start <- return (0,0)
        first <- landLeft 2 start
        second <- landRight 2 first
        Nothing
        landLeft 1 second

    main = print routine
