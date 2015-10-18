module Main (main) where

import Prelude hiding (max)
import Data.Maybe
import CsvParser
import Cards
import System.Environment

cardsFromFile :: FilePath -> IO [Card]
cardsFromFile path = do
    contents <- readFile path
    let csv = parse contents
    case csv of
      (Right csv') -> return $ catMaybes $ map toCard csv'
      (Left err) -> putStrLn (show err) >> return []

toCard :: [(String, String)] -> Maybe Card
toCard [("name", n), ("cost", c), ("magic", m)] =
    Just $ Card n (read c) (read m)
toCard _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] -> do
        cards <- cardsFromFile filename
        let max :: Maybe [Card]
            max = maxCombination cards
        case max of
          Nothing -> putStrLn "No max"
          (Just max') -> do
            let totalMagic = totalForCollection max'
                totalCost = foldr (+) 0 $ map cost max'
            putStrLn $ "Total magic: " ++ show totalMagic
            putStrLn $ "Total cost: " ++ show totalCost
            mapM_ (putStrLn . name) max'
      _ -> error "No filename given"
