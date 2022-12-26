module KotKodacs where
import Data.Char

-- Thx Doboz for the idea <3

-- Codetable, type synonym, helpers --------------------------------------------------

-- Type synonym: Binary numbers are represented by a list of Ints
type BinaryNum = [Int]


-- UTF8 codetable
utf8 :: [(Char, Int)]
utf8 = zip (['!'..'~'] ++ ['Á', 'É', 'Í', 'Ó', 'Ö', 'Ú', 'Ü', 'á', 'é', 'í', 'ó', 'ö', 'ú', 'ü', 'Ő', 'ő', 'Ű', 'ű']) ([33..126] ++ [193, 201, 205, 211, 214, 218, 220, 225, 233, 237, 243, 246, 250, 252, 336, 337, 368, 369])


-- Kot-kodács helper
kotkodacsHelper :: String
kotkodacsHelper = "kot = 0 and kodacs = 1"


decodeHelper :: String
decodeHelper = "decodeFromBinary: Decodes from a list of binary numbers; decodeKotKodacs: Decodes from kot-kodacs list; decodeNumberToMessage: Decodes from a simple String of zeros and ones"


-- Decode part --------------------------------------------------

-- Decodes from a simple String of zeros and ones
decodeNumberToMessage :: String -> String
decodeNumberToMessage list
    | notCorrectLength list = error "Incorrect length!"
    | otherwise = decodeFromBinary $ partsToNumbers list


notCorrectLength :: String -> Bool
notCorrectLength [] = True
notCorrectLength l = (length l) `mod` 8 /= 0


partsToNumbers :: String -> [BinaryNum]
partsToNumbers list = str $ init (eightParts 1 list)
    where
        str [] = []
        str (x:xs) = (strToInt x) : str xs


strToInt :: String -> BinaryNum
strToInt [] = []
strToInt (x:xs) = (digitToInt x) : strToInt xs


eightParts :: Int -> String -> [String]
eightParts _ [] = [[]]
eightParts c (x:xs)
    | c `mod` 8 == 0 = (x : []) : eightParts (c+1) xs
    | otherwise = (x : a) : as
    where
        (a:as) = eightParts (c+1) xs



-- Decodes from kot-kodács list
decodeKotKodacs :: [[String]] -> String
decodeKotKodacs list = decodeFromBinary (f list)
    where
        f :: [[String]] -> [BinaryNum]
        f [] = []
        f (x:xs) = chickenToBinary x : f xs


-- Decodes from a list of binary numbers
decodeFromBinary :: [BinaryNum] -> String
decodeFromBinary list = numberToLetter (numberList list 0 [])


-- Creats a list of decimal numbers from a list of binary numbers
numberList :: [BinaryNum] -> Int -> BinaryNum -> [Int]
numberList [] _ _ = []
numberList (x:xs) c ys
    | howManyBytes x == 1 = toDecimal x : numberList xs 0 []
    | howManyBytes x > 1 = numberList xs ((howManyBytes x) - 1) (partOfNumber x)
    | howManyBytes x == 0 && c == 1 = toDecimal (ys ++ (partOfNumber x)) : numberList xs 0 []
    | otherwise = numberList xs (c-1) (ys ++ (partOfNumber x))


-- Leading byte
howManyBytes :: BinaryNum -> Int
howManyBytes (0:xs) = 1
howManyBytes (1:1:0:xs) = 2
howManyBytes (1:1:1:0:xs) = 3
howManyBytes (1:1:1:1:0:xs) = 4
howManyBytes (1:1:1:1:1:0:xs) = 5
howManyBytes (1:1:1:1:1:1:0:xs) = 6
howManyBytes [1,1,1,1,1,1,1,0] = 7
howManyBytes _ = 0


-- Leading byte (part to eval)
-- Continuation byte (part to eval)
partOfNumber :: BinaryNum -> BinaryNum
partOfNumber [] = []
partOfNumber (1:1:0:xs) = xs
partOfNumber (1:1:1:0:xs) = xs
partOfNumber (1:1:1:1:0:xs) = xs
partOfNumber (1:1:1:1:1:0:xs) = xs
partOfNumber (1:1:1:1:1:1:0:xs) = xs
partOfNumber [1,1,1,1,1,1,1,0] = []
partOfNumber (1:0:xs) = xs


-- Kot-kodács to binary number
chickenToBinary :: [String] -> BinaryNum
chickenToBinary [] = []
chickenToBinary ("kot" : xs) = 0 : chickenToBinary xs
chickenToBinary ("kodács" : xs) = 1 : chickenToBinary xs


-- Binary to decimal
toDecimal :: BinaryNum -> Int
toDecimal list = h (reverse list) 0
    where
        h :: [Int] -> Int -> Int
        h [] _ = 0
        h (1:xs) c = 2 ^ c + (h xs (c+1))
        h (_:xs) c = h xs (c+1)


-- A list of decimal numbers to string
numberToLetter :: [Int] -> String
numberToLetter list = map numberCode list


-- Decimal number to letter
numberCode :: Int -> Char
numberCode num
    | null (f num) = '?'
    | otherwise = fst (head (f num))
    where
        f a = filter (\(x,y) -> y == a) utf8


-- Encode part --------------------------------------------------

-- Encodes the message
encode :: String -> [[String]]
encode [] = []
encode list = map binaryToChicken (map toBinary (letterToNumber list))


-- Binary number to kot-kodács
binaryToChicken :: BinaryNum -> [String]
binaryToChicken [] = []
binaryToChicken (0:xs) = "kot" : binaryToChicken xs
binaryToChicken (1:xs) = "kodacs" : binaryToChicken xs


-- Decimal to binary
toBinary :: Int -> BinaryNum
toBinary 0 = [0]
toBinary n = reverse (h n)
    where
        h 0 = []
        h n = (n `mod` 2) : h (n `div` 2)


-- String to a list of decimal numbers
letterToNumber :: String -> [Int]
letterToNumber list = map letterCode list


-- One letter to decimal number
letterCode :: Char -> Int
letterCode l
    | null (f l) = 0
    | otherwise = snd (head (f l))
    where
        f a = filter (\(x,y) -> x == a) utf8




