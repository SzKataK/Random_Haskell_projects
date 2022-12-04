module BeepBoop where

-- Kódtábla
ascii :: [(Char, Int)]
ascii = zip ([' ','!',',','.','?'] ++ ['A'..'Z'] ++ ['a'..'z']) ([32,33,44,46,63] ++ [65..90] ++ [97..122])


-- Encode part

-- Betűből ascii
letterToNumber :: String -> [Int]
letterToNumber list = map numberCode list
    where
        numberCode :: Char -> Int
        numberCode x = head ([b | (a,b) <- ascii, x==a])


-- Ascii-ből biáris
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (h n)
    where
        h 0 = []
        h n = (n `mod` 2) : h (n `div` 2)


-- Binárisból beep-boop
toBeeps :: [Int] -> [String]
toBeeps [] = []
toBeeps (0:xs) = "Beep" : toBeeps xs
toBeeps (1:xs) = "Boop" : toBeeps xs


-- Üzenet azaz encode
encode :: String -> [[String]]
encode [] = [[]]
encode list = map (toBeeps . toBinary) (letterToNumber list)



-- Decode part

-- Beep-boop-ból bináris
beepsToBinary :: [String] -> [Int]
beepsToBinary [] = []
beepsToBinary ("Beep":xs) = 0 : beepsToBinary xs
beepsToBinary ("Boop":xs) = 1 : beepsToBinary xs


-- Binárisból decimális
toDecimal :: [Int] -> Int
toDecimal list = h (reverse list) 0
    where
        h :: [Int] -> Int -> Int
        h [] _ = 0
        h (1:xs) c = 2 ^ c + (h xs (c+1))
        h (_:xs) c = h xs (c+1)
        

-- Számból betű
numberToLetter :: [Int] -> String
numberToLetter list = map letterCode list
    where
        letterCode :: Int -> Char
        letterCode x = head ([a | (a,b) <- ascii, x == b])


-- Decode
decode :: [[String]] -> String
decode list = numberToLetter (map (toDecimal . beepsToBinary) list)

