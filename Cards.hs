module Cards (
  Card(..)
, maxCombination
, totalForCollection
) where

data Card = Card { name :: String
                 , cost :: Int
                 , magic :: Int
                 }
                 deriving (Show, Eq)

maxMaxNumberOfCards :: Int
maxMaxNumberOfCards = 15

maxCombination :: Int -> [Card] -> Maybe [Card]
maxCombination maxTotalCost cards = foldr transform Nothing [1 .. maxMaxNumberOfCards]
  where
    transform :: Int -> Maybe [Card] -> Maybe [Card]
    transform size biggestSoFar =
      case biggestSoFar of
        Nothing -> newMax
        (Just biggestSoFar') -> case newMax of
                                  Nothing -> Just biggestSoFar'
                                  (Just newMax') -> Just $ biggest newMax' biggestSoFar'
      where
        newMax :: Maybe [Card]
        newMax = maxWith (totalForCollection maxTotalCost) $ sublists cards size

        biggest :: [Card] -> [Card] -> [Card]
        biggest x y = if totalForCollection maxTotalCost x > totalForCollection maxTotalCost y
                        then x
                        else y

maxWith :: (a -> Int) -> [a] -> Maybe a
maxWith f xs = foldr transform Nothing xs
  where
    transform a Nothing = Just a
    transform a (Just maxSoFar) = if (f a) > (f maxSoFar)
                                    then (Just a)
                                    else (Just maxSoFar)

totalForCollection :: Int -> [Card] -> Int
totalForCollection maxTotalCost cards = if totalCost > maxTotalCost
                                          then 0
                                          else totalMagic
  where
    totalCost = foldr (+) 0 $ map cost cards
    totalMagic = foldr (+) 0 $ map magic cards

sublists :: [a] -> Int -> [[a]]
sublists  _     0 = [[]]
sublists  []    _ = []
sublists (x:xs) n = sublists xs n ++ map (x:) (sublists xs $ n - 1)
