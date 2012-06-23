main = interact rmComment

rmComment :: String -> String
rmComment "" = ""
rmComment ('#' : rest) = tail $ dropWhile (/= '\n') $ rmComment rest
rmComment (c : rest) = c : rmComment rest
