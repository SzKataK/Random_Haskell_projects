module DecodeRSA where

-- Megfejtendő: [83, 162, 83, 46, 36, 162, 83, 83, 175]

-- Kódtábla és kulcsok

-- Kódtábla
codeTable :: Integral a => [(a, Char)] 
codeTable = zip [1..10] ['A', 'E', '?', 'G', 'K', 'R', 'S', 'T', 'Z', '?']

-- Privát kulcs
privateKey :: Integral a => (a,a)
privateKey = (7,187)

--------------------------------------------
-- Decode part

-- Visszaadja a számhoz a maradékosztályt
kongruList :: Integral a => [a] -> [a]
kongruList list = map kongru list
    where
        kongru x = (x ^ (fst privateKey)) `mod` (snd privateKey)

-- Számból betű a kódtábla alapján
kongruChar :: Integral a => a -> Char
kongruChar x = snd (head correct)
        where
            correct = (filter (\elso -> fst elso == x) codeTable)

-- Megkapja a számok listáját és visszaalakítja szöveggé
decode :: Integral a => [a] -> String
decode list = map kongruChar (kongruList list)

