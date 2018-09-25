module Main where

import           Data.List       (isPrefixOf)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  c <- getContents
  let s = unlines $ map (buildDropSql . extraceTableName)
            $ filter (not . contain "id_seq")
            $ lines c
  putStrLn s

type TableName = String


extraceTableName :: String -> TableName
extraceTableName= flip (!!) 1 . map trim . splitOn "|"

buildDropSql :: TableName -> String
buildDropSql tn = "drop table " ++ tn ++ ";"

trimRight = dropWhile ((==) ' ')
trimLeft  = reverse . trimRight . reverse
trim = trimRight . trimLeft

contain :: (Eq a) => [a] -> [a] -> Bool
contain _ [] = False
contain phrase token@(c:cs) = if isPrefixOf phrase token
                                 then True
                                 else contain phrase cs

