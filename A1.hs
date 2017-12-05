--James Webb
module A1 where 
    import Data.Char
    import Data.List
    import AssignmentHelp

    --defining the plain and cipher lists
    plain :: [Char]
    plain = ['A'..'Z']
    cipher :: [Char]
    cipher = ['E','K','M','F','L','G','D','Q','V','Z','N','T','O','W','Y','H','X','U','S','P','A','I','B','R','C','J']

    type Cipher = String

    --checks if a cipher contails all letters A-Z in upper case (or just that it matches the plain list of chars)
    validateCipher :: [Char] -> [Char] -> Bool
    validateCipher c p
        | c == [] = False
        | p == (sort c) = True --if they are equal then the cipher is valid
        | otherwise = False --if they are not equal then the cipher is not valid

    --Converts a single plain char to the encoded char including the offset
    encode :: Char -> [Char] -> Int -> Char
    encode plainChar cipherArray offset = (cycle cipherArray !! ((fromMaybe (elemIndex plainChar plain)) + ((-offset) `mod` 26)))


    --ORIGINAL
    --Converts a single encoded char to the plain char inc the offset
    reverseEncode :: Char -> String -> Int -> Char
    reverseEncode x r n =
      let
        p = fromMaybe (elemIndex x r) -- position of x in cipher
        q = mod (p+n) 26 -- position after offset
      in
        plain!!q -- index into alphabet


    --Encodes each char in a string and returns the encoded string (or [Char] - same thing)
    encodeMessage :: String -> [Char] -> Int -> [Char]
    encodeMessage [] c o = [] --if there is nothing in the message return an empty list
    encodeMessage (x:xs) c o = encode x c o : encodeMessage xs c o

    --Takes an encoded string and returns the plain string
    reverseEncodeMessage :: String -> [Char] -> Int -> [Char]
    reverseEncodeMessage [] c o = [] --if there is nothing in the message return an empty list
    reverseEncodeMessage (x:xs) c o = reverseEncode x c o : reverseEncodeMessage xs c o

    --checks if the int in the second element is bigger than the int in the right element
    isLeftBigger :: (Char, Int) -> (Char, Int) -> Bool
    isLeftBigger (c0, i0) (c1, i1) = if (i0 > i1) then True else False
   
   
    --will count the letters in a string and work out their percentages
    letterStats :: String -> [(Char, Int)]
    letterStats message = do
        let groupedLetters = group (sort message)
            singleLetters = map head groupedLetters
            frequencies = map length groupedLetters
            percentages = map (/ intToFloat (length message)) (map intToFloat frequencies)
            percentagesInt = map (*100) percentages
            lStats = zip singleLetters (map round percentagesInt) :: [(Char, Int)]
            orderedLStats = mergesort isLeftBigger lStats
            orderedLStats :: [(Char, Int)]
        return orderedLStats !! 0 -- :

        --let unsorted = zip singleLetters (map round percentagesInt)
        --return (mergesort ((isLeftBigger), (unsorted !! 0)))

    --this method will take in a key and a single encoded char
    decodeChar :: Char -> [(Char, Char)] -> Char
    decodeChar cipherChar key = do
        let unzipKey = unzip key --unzip the key (I think it makes it more effecient to do that once here)
        if (elem cipherChar (snd unzipKey)) then do --if the char exists in the cipher
            let i = fromMaybe (elemIndex cipherChar (snd unzipKey)) --gets the index of the char
            u2Lo ((fst unzipKey) !! i) --returns the lowercase plain char of at index i
        else --if the char doesnt exist in the cipher then just return the original char
            cipherChar


        --return (p !! i) !! 0 --because its a single char thats a string

    --partialDecode [('A','B'), ('B', 'C'), ('C', 'A')] "BCA" 
    --calles decodeChar for each letter in the string recursivly
    partialDecode :: [(Char, Char)] -> String -> String
    partialDecode key [] = [] --when we finish all the letters in the string
    partialDecode key (x:xs) = decodeChar x key : partialDecode key xs

    --solving the mystery code, I used
    --partialDecode [('I','Q'),('T','J'),('S','A'),('E','W'),('A','X'),('G','M'),('Y','R'),('O','F'),('M','P'),('P','V'),('R','E'),('B','B'),('K','Z'),('U','D'),('N','Y'),('C','L'),('H','C'),('V','K'),('D','H'),('L','N'),('F','T'),('Z','U'),('T','S')] 
    --"QJAWXARJFBEWXZXADBAJQJDJQFYLQVCWEVEFKQHWHRFDCXKWXNFYMWYFDMCPWAAXMWAJFVNWJAPXZWJCQAFYWXNQJJNWBQJNFYMWEAJFVFZQJACFDNHBWJCWEQMCJAFEJFTAQUWYFSAJFVPXRBWYFJNWJAQYLEWXAWJCWPWAAXMWNWYMJCXBQJPFEWAJFV"


    --vvvv BELOW ARE OLD FNs THAT I HAVE SINCE REFACTORED vvvv


{-    --counts all occurences of a letter and then calls itself again without that letter until nothing remains
    --returns a sorted list of letters and their f
    letterFreqs :: String -> [(Char, Float)]
    letterFreqs [] = [] --return nothing
    letterFreqs (x:xs) = frequency where
        frequency = ((x, intToFloat (length (filter (==x) (x:xs)))) : (letterFreqs (filter (/=x) xs))) 

    --gets the frequencies, splits the chars away, maps (/ length) to get percentage and zips back together
    --this method would be redundent if I could add another parameter to letterFreqs to pass down the original length of the message
    letterStats :: String -> [(Char, Int)]
    letterStats [] = [] --return nothing
    letterStats message = do
        let freqs = letterFreqs message
        let split = unzip freqs
        let percentages = map (/(intToFloat (length message))) (snd split)
        let stats = zip (fst split) (map round (map (*100) percentages))
        return stats !! 0 --when you unzip you create another list but we only work with the first element
-}


--Original encodeMessage before it was refactored
{-
    --recurses through every letter in the string calling fn encode for each letter
    encodeMessage :: String -> [Char] -> Int -> [Char]
    encodeMessage message c o = 
        if (length message < 1) 
        then []
        else if (length message > 1)
        then encode (head message) c o : encodeMessage (drop 1 message) c o
        else encode (head message) c o : []
-}



{-
    --encodeMessage :: String -> [Char] -> Int -> String
    --encodeMessage message c offset = map (encode (plain, cipher, offset)) message 


    --Takes a whole message and recursivly calls itself to encode each letter of the message
    encodeMessage :: String -> [Char] -> Int -> String
    encodeMessage message cipherArray offset = encodedMessage
        let isListEmpty = null drop 1 message
        if (isListEmpty == True)
            then encodedMessage = null 
            else encodedMessage = (encode (head message) cipher offset) : (drop 1 message))
-}          
            


{-
    encodeMessage message cipherArray offset
        | (null message == True) = "" + 
        | otherwise = encodeMessage
-}






















