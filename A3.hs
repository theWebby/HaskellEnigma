--James Webb
module A3 where
    import Data.Char
    import Data.List
    import EnigmaAndMenu
    import AssignmentHelp

    type SteckerPair = (Char, Char)


    --    Below is some defined variables (of my own and from bombeTesting16) for easy access.

    crib :: Crib
    crib = zip "TURINGBOMBEHASKELLSIMULATIONSTOP" -- "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP" -- "WETTERVORHERSAGEBISKAYA"
               "FNWGDVEHEHJXCGOTOHQLELJOAGABOIDL"-- "RCQRSVHNYQHLVKLELFYSYCCLMKHUFXMVYVREFL" -- "RWIVTYRESXBFOGKUHQBAISE"

    --defined in A2
    steckerBoard :: Stecker
    steckerBoard = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]

    steckeredEnigma :: Enigma
    steckeredEnigma = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB steckerB

    p1 = "AIDEGHMC"
    
    -- and the encripted message is
    
    x1 = "IDEGHMCL"
    
    -- and the crib is the whole message i.e.
    
    c1 = zip p1 x1

    dcs_header = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"

    c2 = "RCQRSVHNYQHLVKLELFYSYCCLMKHUFXMVYVREFLHZOLRCBRHWPQDUONZWOGRTYKAUW"

    ht = "TURINGBOMBEHASKELLSIMULATIONSTOP"

    c3= "FNWGDVEHEHJXCGOTOHQLELJOAGABOIDLXIGKFISZUZCAQNUWKXUMSWTYMBIDZF"







    --Adds a new steckerPair to a Stecker if it is valid
    steckerAdd :: SteckerPair ->Stecker-> Maybe Stecker
    steckerAdd (plain, cipher) stecker
        | (elem (plain, cipher) k) || (elem (cipher, plain) k) = Just stecker -- the steckerPair already exists
        | length k == 0 = Just ([(plain, cipher)]++stecker) -- If it isnt in K then we can add it (see where k = ...)
        | otherwise = Nothing
            where k = filter(\(a,b)-> a==plain || b==cipher || a==cipher || b==plain) stecker -- all elements of stecker that contain elements of the steckerPair


    --Recurse through the menu adding new stecker pairs until a contradiction is found of the menu is exausted
    followMenu :: Crib->Menu->Stecker->Offsets->Maybe Stecker
    followMenu _ _ [] _ = Nothing
    followMenu _ [] steckerBoard _ = Just steckerBoard -- the steckerboard represents a solution
    followMenu crib (position:positions) stecks offsets
         | newStecker == Nothing = Nothing
         | (newStecker /= Nothing) = followMenu crib positions (fromMaybe(newStecker)) offsets --recurse while the new stecker is valid
         | otherwise = Nothing
         where
           p = (fst (crib !! position)) -- Plain char
           q = stecker p steckerBoard -- Steckered plain Char
           r = enigmaEncodeA q (SimpleEnigma rotor1 rotor2 rotor3 reflectorB) (offsetN offsets (position + 1)) -- Encoded stecker char
           c = (snd (crib !! position)) -- the cipher char
           newStecker = steckerAdd (c,r) steckerBoard -- new stecker with the new stecker pair added


    -- Recurses to find a steckerBoard that is valid for a given set of initial offsets
    findStecker :: Crib->Menu->Stecker->Offsets->Maybe Stecker
    findStecker crib menu steckerBoard offsets
        | y == prevInAlphabet x = Nothing
        | thisStecker == Nothing = findStecker crib menu [(x, nextInAlphabet y)] offsets
        | otherwise = thisStecker
        where x = fst (steckerBoard !! 0)
              y = snd (steckerBoard !! 0)
              thisStecker = followMenu crib menu steckerBoard offsets -- follows the menu for the current initial stecker values

    -- Recurses to find a steckerboard and a set of offsets that are valid
    breakEA :: Crib->Menu->Stecker->Offsets->Maybe(Offsets, Stecker)
    breakEA crib menu steckerBoard offsets
        | thisStecker /= Nothing = Just (offsets, (fromMaybe thisStecker))
        | offsets == (25,25,25) = Nothing
        | otherwise = breakEA crib menu steckerBoard (offset_step offsets)
        where thisStecker = findStecker crib menu steckerBoard offsets -- finds the stecker for the current values

    -- finds the longest menu for a given crib and trys to find a set of offsets and a valid steckerboard
    breakEnigma :: Crib->Maybe(Offsets, Stecker)
    breakEnigma crib = breakEA crib menu steckerBoard (0,0,0) -- starts looking from (0,0,0) and ends at (25,25,25)
        where menu = longestMenu crib
              firstChar = fst (crib !! (menu !! 0)) 
              steckerBoard = [(firstChar, firstChar)] -- making an assumption about the initial stecker values


    -- Finds the next character in the alphabet and goes back to 'A' if the given value is 'Z'
    nextInAlphabet :: Char->Char
    nextInAlphabet 'Z' = 'A'
    nextInAlphabet c = chr (ord c + 1)

    -- Finds the previous character in the alphabet and goes back to 'Z' if the given value is 'A'
    prevInAlphabet :: Char->Char
    prevInAlphabet 'A' = 'Z'
    prevInAlphabet c = chr (ord c - 1)