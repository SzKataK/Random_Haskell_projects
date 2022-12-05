module BeepBoop where

-- HUN: Kódtábla
-- ENG: Codetable
ascii :: [(Char, Int)]
ascii = zip ([' ','!',',','.','?'] ++ ['A'..'Z'] ++ ['a'..'z']) ([32,33,44,46,63] ++ [65..90] ++ [97..122])

----------------------------------------
-- Encode part

-- HUN: Megkeresi a betűkhöz tartozó Ascii-kódokat
-- ENG: Finds the Ascii code of each character in a String
letterToNumber :: String -> [Int]
letterToNumber list = map numberCode list
    where
        numberCode :: Char -> Int
        numberCode x = head ([b | (a,b) <- ascii, x == a])

-- HUN: Az Ascii-kódot (decimális szám) átváltja bináris számra, amit egy Int lista reprezentál
-- ENG: Converts the Ascii code (decimal number) to a binary number represented by an Int list
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (h n)
    where
        h 0 = []
        h n = (n `mod` 2) : h (n `div` 2)

-- HUN: A bináris szám jegyeit Beep-re és Boop-ra cseréli
-- ENG: It replaces the digits of the binary number with Beep and Boop
-- Beep = 0; Boop = 1
toBeeps :: [Int] -> [String]
toBeeps [] = []
toBeeps (0:xs) = "Beep" : toBeeps xs
toBeeps (1:xs) = "Boop" : toBeeps xs

-- HUN: Kódolja az üzenetet
-- ENG: Encodes the message
encode :: String -> [[String]]
encode [] = [[]]
encode list = map (toBeeps . toBinary) (letterToNumber list)

----------------------------------------
-- Decode part

-- HUN: A Beep-eket és a Boop-okat 0-ra és 1-re cseréli
-- ENG: It replaces the Beeps and the Boops with 0 and 1
-- Beep = 0; Boop = 1
beepsToBinary :: [String] -> [Int]
beepsToBinary [] = []
beepsToBinary ("Beep":xs) = 0 : beepsToBinary xs
beepsToBinary ("Boop":xs) = 1 : beepsToBinary xs

-- HUN: A bináris számot átváltja decimálisra
-- ENG: Converts binary numbers to decimal numbers
toDecimal :: [Int] -> Int
toDecimal list = h (reverse list) 0
    where
        h :: [Int] -> Int -> Int
        h [] _ = 0
        h (1:xs) c = 2 ^ c + (h xs (c+1))
        h (_:xs) c = h xs (c+1)
        
-- HUN: Megkeresi a számhoz tartozó Ascii-kódot
-- ENG: Finds the Ascii code for the number
numberToLetter :: [Int] -> String
numberToLetter list = map letterCode list
    where
        letterCode :: Int -> Char
        letterCode x = head ([a | (a,b) <- ascii, x == b])

-- HUN: Dekódolja az üzenetet
-- ENG: Decodes the message
decode :: [[String]] -> String
decode list = numberToLetter (map (toDecimal . beepsToBinary) list)
