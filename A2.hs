--James Webb
module A2 where 
    import Data.Char
    import Data.List
    import Data.Function
    import AssignmentHelp
    import A1

    type Rotor = String
    type Plain = String
    type Reflector = [(Char, Char)]
    type Offsets = [Int]
    type SteckerPair = (Char, Char)
    type SteckerBoard = [SteckerPair]
    type SimpleEnigma = ([Rotor], Reflector)
    type SteckeredEnigma = (SteckerBoard, [Rotor], Reflector)
    type Crib = [(Char, Char)]
    type Menu = [Int]

    data Enigma = SimpleEnigma ([Rotor], Reflector) | SteckeredEnigma (SteckerBoard, [Rotor], Reflector)

    --Defininition of the reflector I have been using -- ANY reflector can be used
    reflector :: Reflector
    reflector = [('A', 'Y'),('B', 'R'),('C', 'U'),('D', 'H'),('E', 'Q'),('F', 'S'),('G', 'L'),('I', 'P'),('J', 'X'),('K', 'N'),('M', 'O'),('T', 'Z'),('V', 'W')]

    --Definition of the steckerboard I used -- ANY steckerboard can be used (max 10 pairs)
    --steckerBoard :: SteckerBoard
    --steckerBoard = [('A', 'B'), ('C', 'D'), ('E', 'F')]

    --Definition of the simple enigma I used -- ANY rotors and ANY reflector may be used
    simpleEnigma :: Enigma
    simpleEnigma = SimpleEnigma ([rotor3, rotor2, rotor1], reflector)
    --                             left   middle  right

    --Definition of the steckered enigma I used -- ANY stecker board, rotors or reflector may be used
    

    -- Definition of the Crib I used -- ANY crib can be constructed
    crib :: Crib
    crib = zip "AIDEGHMC" -- "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP" -- "WETTERVORHERSAGEBISKAYA"
               "IDEGHMCL" -- "RCQRSVHNYQHLVKLELFYSYCCLMKHUFXMVYVREFL" -- "RWIVTYRESXBFOGKUHQBAISE"

    -- --Finds the longest menu in a given crib
    -- longestMenu :: Crib->Menu
    -- longestMenu c = maximumBy (compare `on` length) (makeMenus c 0) --make menus from first element

    -- --Returns a list of all menus in a crib from given index start point (0 = all menus)
    -- makeMenus :: Crib->Int->[Menu]
    -- makeMenus c i
    --     | i < length (fst c) = makeMenu c i [] : makeMenus c (i+1)
    --     | otherwise = [] 

    -- --do while next link isnt first element in list (might have to pass list through too aswell as return it)
    -- makeMenu :: Crib->Int->[Int]->[Int]
    -- makeMenu c s [] 
    --     | i /= -1 = s : i : makeMenu c i [s]
    --     | otherwise = [] 
    --     where i = makeLink c s
    -- makeMenu c s m 
    --     | ((i /= -1) && ((elem i m) == False)) = i : makeMenu c i (i : m)
    --     | otherwise = init m 
    --     where i = makeLink c s

    -- --find the index of a chipher char in plain in a crib
    -- makeLink :: Crib->Int->Int
    -- makeLink c i 
    --     | (elem ((snd c !!) i) (fst c)) = fromMaybe (elemIndex ((snd c !!) i) (fst c))
    --     |otherwise = -1

    --enigmaEncodes each char in a string returning the enigmaEncoded message
    enigmaEncodeMessage :: String->Enigma->Offsets->String
    enigmaEncodeMessage [] e po = []
    enigmaEncodeMessage (x:xs) e po = enigmaEncode x e o : enigmaEncodeMessage xs e (advanceRotors o) where
        o = advanceRotors po

    --passes a char through all rotots (right to left), reflects it and then back through rotors (left to right)
    enigmaEncode :: Char->Enigma->Offsets->Char
    enigmaEncode c (SimpleEnigma (rotors, r)) o = reverseRotors (reflect (applyRotors c (reverse rotors) (reverse o)) r) rotors o
    enigmaEncode c (SteckeredEnigma (stec, rotors, r)) o = stecker (reverseRotors (reflect (applyRotors sc (reverse rotors) (reverse o)) r) rotors o) stec where
        sc = stecker c stec

    --encodes a char through all rotors in a list
    applyRotors :: Char->[Rotor]->Offsets->Char
    applyRotors c [] [] = c
    applyRotors c (rotor:rotors) (offset:offsets) = applyRotors (encode c rotor offset) rotors offsets

    --reverse encodes a char through all rotors in a list
    reverseRotors :: Char->[Rotor]->Offsets->Char
    reverseRotors c [] [] = c
    reverseRotors c (rotor:rotors) (offset:offsets) = reverseRotors (reverseEncode c rotor offset) rotors offsets

    --outputs corrosponding letter from the reflector
    reflect :: Char->Reflector->Char
    reflect c r 
        | (elem c (fst unzipReflector)) = snd unzipReflector !! (fromMaybe (elemIndex c (fst unzipReflector)))
        | otherwise = fst unzipReflector !! (fromMaybe (elemIndex c (snd unzipReflector)))
        where unzipReflector = unzip r

          
    --same as reflect but if the char for steckering, if the letter isn't found the input char is returned unchanged
    stecker :: Char->SteckerBoard->Char
    stecker c sb 
        | (elem c (fst unzipSteckerBoard)) = snd unzipSteckerBoard !! (fromMaybe (elemIndex c (fst unzipSteckerBoard)))
        | (elem c (snd unzipSteckerBoard)) = fst unzipSteckerBoard !! (fromMaybe (elemIndex c (snd unzipSteckerBoard)))
        | otherwise = c
        where unzipSteckerBoard = unzip sb



    --moves the rotors along one place; pattern matching for resets then incrementing otherwise
    advanceRotors :: [Int]->[Int]
    advanceRotors [25, 25, 25] = [0, 0, 0]
    advanceRotors [x, 25, 25] = [1, 0, 0]
    advanceRotors [x, y, 25] = [x, y+1, 0]
    advanceRotors [0, 0, 0] = [0, 0, 1]
    advanceRotors [x, y, z]
        | z < 25 = [x, y, z+1]
        | y < 25 && y /= 0 = [x, y+1, z]
        | x < 25 && x /= 0 = [x+1, y, z]






    --defined some function for accessing a tuple with more than 2 elements
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    snd3 :: (a, b, c) -> b
    snd3 (_, x, _) = x

    trd3 :: (a, b, c) -> c
    trd3 (_, _, x) = x

    --converts a steckered enigma to a simple enigma
    --steckerToSimple :: SteckeredEnigma->Enigma
    --steckerToSimple e = SimpleEnigma (snd3 e, trd3 e)


     -----------------------------------------------------
    -- Enigma encoding

    {- encode single Char
    x the char
    e an Enigma
    os offsets triple
    lr,mr,rr the rotors
    ref reflector
    sb steckerboard
    -}

    enigmaEncode :: Char->Enigma->Offsets->Char -- encript single Char
    enigmaEncode x e os = enigmaEncodeA x e (offset_step os) -- advance rotors then call A fn to do encoding

    -- simple enigma

    enigmaEncodeA x (SimpleEnigma lr mr rr ref) (ol,om,or)=
        reverseEncode rr or     -- left rotor returning
        (reverseEncode mr om    -- middle rotor returning
         (reverseEncode lr ol  -- left rotor returning
          (reflect -- fixed reflector
           (encode lr ol -- left rotor forward
             (encode mr om-- mid rotor forward
                    (encode rr or x) -- right rotor forward
             )
           )
           ref
          )
         )
        )

    -- steckered enigma

    enigmaEncodeA x (SteckeredEnigma lr mr rr ref sb) (ol,om,or) = 
        stecker -- stecker the output from the following
         (reverseEncode rr or     -- left rotor returning
          (reverseEncode mr om    -- middle rotor returning
           (reverseEncode lr ol  -- left rotor returning
            (reflect -- fixed reflector
             (encode lr ol -- left rotor forward
              (encode mr om-- mid rotor forward
                     (encode rr or (stecker x sb)) -- right rotor forward
              )
             )
            ref
            )
           )
          )
         )
         sb

    {- encode a message
    (h:t) the message
    nos offsets after stepping
    nc encode of h
    -}

    enigmaEncodeMessage :: String->Enigma->Offsets->String -- encript message 

    enigmaEncodeMessage [] (SimpleEnigma _ _ _ _) _ = []       -- 2 versions for 2 constructors
    enigmaEncodeMessage [] (SteckeredEnigma _ _ _ _ _) _ = []

    enigmaEncodeMessage (h:t) e os =
        let 
         nos = offset_step os  -- step the rotors
         nc = enigmaEncodeA h e nos -- encode the first Char, without advancing again
        in (nc: enigmaEncodeMessage t e nos) -- encode the rest

    ---------------------------------------------------------------------------------------------
    -- ENCODING CHARS, from assignment 1
    -- MOVING CIPHER LEFT, NOT RIGHT, AS REQUIRED FOR ASS3
    -- forward encoding
    -- with an offset

    encode :: Cipher->Int->Char->Char -- put Char to be encoded as last arg for partial fn

    encode  r n x =  
        let
        p=mod ((alphaPos x) + n)  26  -- position of the given ch x after rotation
        in r!!p

    -- reverse encoding

    reverseEncode :: Cipher->Int->Char->Char

    reverseEncode  r n x =
        let
        p = findPos x r
        q = mod (p-n) 26
        in
        alphabet!!q 


    -- position of a given character in a rotor

    findPos :: Char ->Cipher->Int

    {- version with explicit recursion
    findPos ch (h:t)
    |(ch==h) = 0
    |otherwise = 1+ (findPos ch t)
    -}

    -- with a comprehension
    findPos ch r = 
        head [p |(c,p) <- (zip r [0..25]), c==ch]

    -- alphabetic posiiton for an uppercase letter
    -- starting @ 0 for 'A'
    -- ord in Data.Char gives ordering for Chars - 'A' is 65

--_________________________


    



    longestMenu :: Crib->Menu
    
    longestMenu crib = maximumBy (\m1 m2 ->(compare (length m1) (length m2))) (findMenus crib) -- OR findMenus3
    
    
    ------------------------------------------------------------
    {- findMenus  find all the menus
       state space search-based - open, closed, select from open, add successors to closed, recurse
       successors expand menu m to right
       passive is menus which won't expand to right, but which can still join to others at left.
       mist check successors for circularity
       could probably do without closed alltogether.
       start with menus length 1 ie. [(x,y)] where encript @ x == plain @y
       findLinks extracts this from crib
    -}
    findMenus :: Crib -> [Menu]
    
    findMenus crib = findMenusA (findLinks crib) [] []  -- set up intiial open, passive, closed
    
    -- recursive search
    findMenusA :: [Menu]->[Menu]-> [Menu]-> [Menu]
    
    -- terminate when open empty
    -- returns passive .. menus on closed have been extended
    findMenusA [] passive closed = passive
    
    -- expand first menu on open
    findMenusA open@(m:rm) passive closed
     |not (null nms) = findMenusA (rm++nms) passive (m:closed) -- successors found, so add m to closed
     |otherwise =  findMenusA rm (m:passive) closed -- no successors, add m to passive, recurse with rest of open
     where
      linkedMenus = [mm|mm@(mm1:_)<-(rm++passive),mm1==last m] -- find menus in open or passive m joins to i.e. tail joins to head
      pms = [m++rlm|(_:rlm)<-linkedMenus] -- possible new menus
      nms = [pm|pm<-pms, noDuplicates pm] -- filter to prevent circular chains
      
      
    --------------------------------------------------------------  
    -- predicate - True if a list has no repeated elements
    -- could use nub
    
    noDuplicates :: Eq a => [a]->Bool
    
    noDuplicates [] = True
    noDuplicates [_] = True
    
    noDuplicates (h:t)
     |(elem h t) = False
     |otherwise = noDuplicates t
     
    ---------------------------------------------------------------------------------
    {- find initial menus of length 2
       these form initial open
       set up plain p and cipher c with indices
       comprehension checks for links & returns index pairs
    -}
    findLinks :: Crib->[Menu]
    
    findLinks crib = [[yi,xi] |(xi,x)<-p,(yi,y)<-c,x==y] 
     where 
      inds = [0..((length crib)-1)]
      (pl,cl)= unzip crib
      p = zip inds pl
      c=  zip inds cl

