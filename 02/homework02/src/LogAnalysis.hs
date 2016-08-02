module LogAnalysis
    ( parse
    , parseMessage
    , insert
    , build
    , inOrder
    , whatWhenWrong
    ) where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s
  | (head.words $ s) == "E"     =
    let (_:y:z:xs) = words s in LogMessage (Error (read y)) (read z) (unwords xs)
  | (head.words $ s) == "I"     =
    let (_:y:xs) = words s in LogMessage Info (read y) (unwords xs)
  | (head.words $ s) == "W"     =
    let (_:y:xs) = words s in LogMessage Warning (read y) (unwords xs)
  | otherwise                   = Unknown s

parse :: String -> [LogMessage]
parse s = fmap parseMessage (lines s)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert m  Leaf                = Node Leaf m Leaf
insert (Unknown _)  t         = t
insert m (Node l (Unknown _) r)
                              = Node l m r
insert m@(LogMessage _ i _) (Node l msg@(LogMessage _ j _) r)
  | i <= j                    = Node (insert m l) msg r
  | otherwise                 = Node l msg (insert m r)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l m r)  = inOrder l ++ [m] ++ inOrder r

-- Exercise 5
whatWhenWrong :: [LogMessage] -> [String]
whatWhenWrong = foldr isWrong []
  where
    isWrong :: LogMessage -> [String] -> [String]
    isWrong (LogMessage (Error x) _ s) ms
      | x >= 50   = ms++[s]
      | otherwise = ms
    isWrong _ ms  = ms

