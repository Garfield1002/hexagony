module Main ( main ) where

import           Control.Monad      ( when )

import qualified Data.Array         as A
import           Data.Array         ( (!), (//) )
import qualified Data.Char          as C
import qualified Data.Map           as Map
import qualified Data.Maybe         as M

import           System.Environment ( getArgs )
import           System.IO          ( hPutStr, isEOF, stderr, stdin, stdout )

import           Text.Printf        ( printf )

-- Utilities
-- Pads a string to a given length using a specific character
pad :: String -> Int -> Char -> String
pad s n c = s ++ replicate (n - n') c
  where
    n' = length s

-- Core
-- A Regular centered hexagon
data Hexagon = Hexagon { maxCommands :: Int, minCommands :: Int, size :: Int }

-- Directions inside a flat-topped hexagon
data Direction = East | West | SouthEast | SouthWest | NorthEast | NorthWest

instance Show Direction where
    show direction = case direction of
        East -> "E"
        West -> "W"
        SouthEast -> "SE"
        SouthWest -> "SW"
        NorthEast -> "NE"
        NorthWest -> "NW"

-- We will be using axial coordinates https://www.redblobgames.com/grids/hexagons/#coordinates-axial
data HexCoordinate =
    HexCoordinate { q :: Integer, r :: Integer, direction :: Direction }

-- The current state of the program
data State = State { memory       :: Map.Map (Integer, Integer) Integer
                   , mp           :: HexCoordinate
                   , ip           :: HexCoordinate
                   , ipIdx        :: Int
                   , ips          :: A.Array Int HexCoordinate
                   , terminated   :: Bool -- Is the execution over
                   , errorMessage :: String -- The reason for termination
                   }

data Program = Program { instructions :: Map.Map (Integer, Integer) Char
                       , state        :: State
                       , size2        :: Integer
                       }

instance Show State where
    show State{memory = memory, mp = mp, ip = ip} = ""

instance Show Hexagon where
    show Hexagon{maxCommands = maxCommands, size = s} =
        show s ++ '\n' : show maxCommands

chn :: Int -> Int
chn n = 3 * n * (n + 1) + 1

-- Centered Hexagonal Numbers: https://oeis.org/A003215
chns :: [(Int, Int)]
chns = [ (chn n, n + 1) | n <- [ 0 .. ] ]

-- Smallest regular centered hexagon that fits a program of given length
-- TODO handle len 0
getHexagon :: Int -> Hexagon
getHexagon programLen = Hexagon { maxCommands = maxCommands
                                , minCommands = (+) 1 . chn $ n - 1
                                , size        = n
                                }
  where
    (maxCommands, n) = head . dropWhile ((< programLen) . fst) $ chns

-- Pads and cleans up a hexagony program with .
cleanProgram :: String -> String
cleanProgram p = pad program maxCommands '.'
  where
    program = filter (\c -> c /= ' ' && c /= '\n') p

    Hexagon{maxCommands = maxCommands} = getHexagon . length $ program

-- Formats a hexagony program
formatProgram :: String -> String
formatProgram p = format' 1 n program
  where
    program = cleanProgram p

    Hexagon{size = n} = getHexagon . length $ program

    format' _ _ "" = ""
    format' i n' p =
        let i' = if n' == 2 * n - 1 then -1 else i
        in
            (reverse . foldl (\s c -> c : ' ' : s) (replicate (2 * n - n') ' ')
             . take n' $ p) ++ '\n' : format' i' (n' + i') (drop n' p)

-- Create an empty hexagon with given dimensions
emptyHexagon :: Int -> String
emptyHexagon n = formatProgram $ replicate (chn (n - 1)) '.'

{-* ======================================================================
                            MEMORY OPERATIONS
====================================================================== *-}
--Gets the value of a memory edge
getMemEdge :: State -> HexCoordinate -> Integer
getMemEdge State{memory = memory} HexCoordinate{q = q, r = r} =
    M.fromMaybe 0 $ Map.lookup (q, r) memory

-- Sets the value of a memory edge
setMemEdge :: State -> HexCoordinate -> Integer -> State
setMemEdge state@State{memory = memory} HexCoordinate{q = q, r = r} value =
    state { memory = Map.insert (q, r) value memory }

memLeft :: HexCoordinate -> HexCoordinate
memLeft HexCoordinate{q = q, r = r, direction = East} =
    HexCoordinate { q = q + 1, r = r + 0, direction = NorthEast }
memLeft HexCoordinate{q = q, r = r, direction = West} =
    HexCoordinate { q = q - 1, r = r + 0, direction = SouthWest }
memLeft HexCoordinate{q = q, r = r, direction = SouthEast} =
    HexCoordinate { q = q + 0, r = r + 1, direction = East }
memLeft HexCoordinate{q = q, r = r, direction = SouthWest} =
    HexCoordinate { q = q - 1, r = r + 1, direction = SouthEast }
memLeft HexCoordinate{q = q, r = r, direction = NorthEast} =
    HexCoordinate { q = q + 1, r = r - 1, direction = NorthWest }
memLeft HexCoordinate{q = q, r = r, direction = NorthWest} =
    HexCoordinate { q = q + 0, r = r - 1, direction = West }

memRight :: HexCoordinate -> HexCoordinate
memRight HexCoordinate{q = q, r = r, direction = East} =
    HexCoordinate { q = q + 0, r = r + 1, direction = SouthEast }
memRight HexCoordinate{q = q, r = r, direction = West} =
    HexCoordinate { q = q + 0, r = r - 1, direction = NorthWest }
memRight HexCoordinate{q = q, r = r, direction = SouthEast} =
    HexCoordinate { q = q - 1, r = r + 1, direction = SouthWest }
memRight HexCoordinate{q = q, r = r, direction = SouthWest} =
    HexCoordinate { q = q - 1, r = r + 0, direction = West }
memRight HexCoordinate{q = q, r = r, direction = NorthEast} =
    HexCoordinate { q = q + 1, r = r + 0, direction = East }
memRight HexCoordinate{q = q, r = r, direction = NorthWest} =
    HexCoordinate { q = q + 1, r = r - 1, direction = NorthEast }

memReverse :: HexCoordinate -> HexCoordinate
memReverse coord@HexCoordinate{direction = East} = coord { direction = West }
memReverse coord@HexCoordinate{direction = West} = coord { direction = East }
memReverse coord@HexCoordinate{direction = NorthEast} =
    coord { direction = SouthWest }
memReverse coord@HexCoordinate{direction = NorthWest} =
    coord { direction = SouthEast }
memReverse coord@HexCoordinate{direction = SouthEast} =
    coord { direction = NorthWest }
memReverse coord@HexCoordinate{direction = SouthWest} =
    coord { direction = NorthEast }

{------------------------------------------------------------------------
                            MOVEMENT OPERATIONS
------------------------------------------------------------------------}
-- Move the current IP forward
moveIP :: Integer -> State -> State
moveIP size
       state@State{ mp = mp
                  , ip = ip@HexCoordinate{q = q, r = r, direction = direction}
                  } =
    let (q', r') = case direction of
            East -> (q + 1, r)
            SouthEast -> (q, r + 1)
            SouthWest -> (q - 1, r + 1)
            West -> (q - 1, r)
            NorthWest -> (q, r - 1)
            NorthEast -> (q + 1, r - 1)
    in
        let (q'', r'') = overflow q' r'
        in
            state { ip = ip { q = q'', r = r'' } }
  where
    overflow q' r'
        | abs q' >= size && r' /= 0 && s' /= 0 = (-q, -s)
        | abs q' >= size && r' == 0 && isMemEdge = (-q, -s)
        | abs q' >= size && r' == 0 = (0, s)
        --
        | abs r' >= size && s' /= 0 && q' /= 0 = (-s, -r)
        | abs r' >= size && s' == 0 && isMemEdge = (0, -r)
        | abs r' >= size && s' == 0 = (r, 0)
        --
        | abs s' >= size && q' /= 0 && r' /= 0 = (-r, -q)
        | abs s' >= size && q' == 0 && isMemEdge = (-r, -q)
        | abs s' >= size && q' == 0 = (r, s)
        --
        | otherwise = (q', r')
      where
        s' = -q' - r'

        s = -q - r

    memEdge = getMemEdge state mp

    isMemEdge = memEdge > 0

-- Reads an Int from STDIN, if no int is found return 0
getInt :: IO Integer
getInt = do
    l <- getInt' True
    return $ head l * foldl (\n d -> 10 * n + d) 0 (tail l)
  where
    getInt' isFst = do
        done <- isEOF
        if done
            then return [ 1, 0 ]
            else do
                c <- getChar
                helper isFst c

    helper :: Bool -> Char -> IO [Integer]
    helper isFst c
        | isFst && c == '+' = (1 :) <$> getInt' False
        | isFst && c == '-' = (-1 :) <$> getInt' False
        | isFst && C.isDigit c = (1 :) . (fromIntegral (C.digitToInt c) :)
            <$> getInt' False
        | C.isDigit c = (fromIntegral (C.digitToInt c) :) <$> getInt' False
        | not isFst = return []
        | otherwise = getInt' True

{------------------------------------------------------------------------
                                    CORE
------------------------------------------------------------------------}
-- Runs a binary operation
binop :: State -> (Integer -> Integer -> Integer) -> State
binop state@State{memory = memory, mp = mp} f = setMemEdge state mp $ f l r
  where
    l = getMemEdge state $ memLeft mp

    r = getMemEdge state $ memRight mp

-- Executes the instruction on the current cell
execute :: State -> Integer -> Char -> IO State
execute state@State{ mp = mp
                   , ip = ip@HexCoordinate{direction = ipDirection}
                   , ipIdx = ipIdx
                   , ips = ips
                   }
        size
        c
    |
        {------------------------- Arithmetic Operations -------------------------}
        C.isDigit c = moveIP' $ setCurMemEdge $ 10 * memEdge
        + fromIntegral (C.digitToInt c)
    |
        -- increments the current memory edge
        c == ')' = moveIP' . setCurMemEdge $ memEdge + 1
    |
        -- decrements the current memory edge
        c == '(' = moveIP' $ setCurMemEdge $ memEdge - 1
    |
        -- sets the current memory edge to the sum of the left and right neighbors.
        c == '+' = moveIP' $ binop state (+)
    |
        --sets the current memory edge to the difference of the left and right neighbors
        -- (left - right).
        c == '-' = moveIP' $ binop state (-)
    |
        -- sets the current memory edge to the product of the left and right neighbors.
        c == '*' = moveIP' $ binop state (*)
    |
        -- sets the current memory edge to the quotient of the left and right neighbors
        -- (left / right, rounded towards negative infinity).
        c == ':' = moveIP' $ binop state quot   -- DIVISION BY 0
    |
        -- sets the current memory edge to the modulo of the left and right neighbors
        -- (left % right, the sign of the result is the same as the sign of right).
        c == '%' = moveIP' $ binop state mod    -- DIVISION BY 0
    |
        -- multiplies the current memory edge by -1
        c == '~' = moveIP' $ setCurMemEdge $ memEdge * (-1)

    |
        {------------------------- IO Operations -------------------------}
            -- reads a single byte from STDIN and sets the current memory edge to its value, or -1 if EOF is reached.
        c == ',' = do
            done <- isEOF
            value <- if done then return (-1) else C.ord <$> getChar
            moveIP' $ setCurMemEdge (fromIntegral value)
    |
            -- reads and discards from STDIN until a digit, a - or a + is found.
            -- Then reads as many characters as possible to form a valid (signed) decimal integer
            -- and sets the current memory edge to its value. Returns 0 once EOF is reached.
        c == '?' = moveIP size . setCurMemEdge <$> getInt
    |
        -- takes the current memory edge modulo 256 (positive) and writes the corresponding byte to STDOUT.
        c == ';' = do
            putChar $ C.chr . fromInteger $ (256 + (memEdge `mod` 256))
                `mod` 256 -- TODO better positive modulus
            moveIP' state
    |
        -- writes the decimal representation of the current memory edge to STDOUT.
        c == '!' = do
            putStr . show $ memEdge
            moveIP' state

    |
        {------------------------- Control flow -------------------------}
        -- `$` is a jump. When executed, the IP completely ignores the next command in its current direction.
        -- This is like Befunge's #
        c == '$' = moveIP' $ moveIP size state

    -- _, |, /, \ are mirrors. They reflect the IP in the direction you'd expect.
    | c == '/' = let d = case ipDirection of
                         East -> NorthWest
                         SouthEast -> West
                         SouthWest -> SouthWest
                         West -> SouthEast
                         NorthWest -> East
                         NorthEast -> NorthEast
                 in
                     moveIP' $ state { ip = ip { direction = d } }
    | c == '\\' = let d = case ipDirection of
                          East -> SouthWest
                          SouthEast -> SouthEast
                          SouthWest -> East
                          West -> NorthEast
                          NorthWest -> NorthWest
                          NorthEast -> West
                  in
                      moveIP' $ state { ip = ip { direction = d } }
    | c == '_' = let d = case ipDirection of
                         East -> East
                         SouthEast -> NorthEast
                         SouthWest -> NorthWest
                         West -> West
                         NorthWest -> SouthWest
                         NorthEast -> SouthEast
                 in
                     moveIP' $ state { ip = ip { direction = d } }
    | c == '|' = let d = case ipDirection of
                         East -> West
                         SouthEast -> SouthWest
                         SouthWest -> SouthEast
                         West -> East
                         NorthWest -> NorthEast
                         NorthEast -> NorthWest
                 in
                     moveIP' $ state { ip = ip { direction = d } }

    -- < and > act as either mirrors or branches, depending on the incoming direction.
    | c == '<' = let d = case ipDirection of
                         East -> if isMemEdge then SouthEast else NorthEast
                         SouthEast -> NorthWest
                         SouthWest -> West
                         West -> East
                         NorthWest -> West
                         NorthEast -> SouthWest
                 in
                     moveIP' $ state { ip = ip { direction = d } }
    | c == '>' = let d = case ipDirection of
                         East -> West
                         SouthEast -> East
                         SouthWest -> NorthEast
                         West -> if isMemEdge then NorthWest else SouthWest
                         NorthWest -> SouthEast
                         NorthEast -> East
                 in
                     moveIP' $ state { ip = ip { direction = d } }
    |
        --  switches to the previous IP (wrapping around from 0 to 5).
        c == '[' = do
            let State{ip = ip'} = moveIP size state
            let newIps = ips // [ (ipIdx, ip') ]
            let newIpIdx = (ipIdx + 5) `mod` 6
            return state { ip    = newIps ! newIpIdx
                         , ipIdx = newIpIdx
                         , ips   = newIps
                         }
    |
        --  switches to the next IP (wrapping around from 5 to 0).
        c == ']' = do
            let State{ip = ip'} = moveIP size state
            let newIps = ips // [ (ipIdx, ip') ]
            let newIpIdx = (ipIdx + 1) `mod` 6
            return state { ip    = newIps ! newIpIdx
                         , ipIdx = newIpIdx
                         , ips   = newIps
                         }

    |
        --takes the current memory edge modulo 6 and switches to the IP with that index.
        c == '#' = do
            let State{ip = ip'} = moveIP size state
            let newIps = ips // [ (ipIdx, ip') ]
            let newIpIdx = fromInteger (memEdge `mod` 6 + 6) `mod` 6
            return state { ip    = newIps ! newIpIdx
                         , ipIdx = newIpIdx
                         , ips   = newIps
                         }

    |
        {------------------------- Memory Manipulation -------------------------}
        -- moves the MP to the left neighbor.
        c == '{' = moveIP' $ state { mp = memLeft mp }
    |
        -- moves the MP to the right neighbor.
        c == '}' = moveIP' $ state { mp = memRight mp }
    |
        -- moves the MP backwards and to the left. This is equivalent to =}=.
        c == '"' =
        moveIP' $ state { mp = memReverse . memRight . memReverse $ mp }
    |
        -- moves the MP backwards and to the right. This is equivalent to ={=.
        c == '\'' =
        moveIP' $ state { mp = memReverse . memLeft . memReverse $ mp }
    |
        -- reverses the direction of the MP.
        -- (This doesn't affect the current memory edge, but changes which edges are considered the left and right neighbor.)
        c == '=' = moveIP' $ state { mp = memReverse mp }
    |
        -- moves the MP to the left neighbor if the current edge is zero or negative and to the right neighbor if it's positive.
        c == '^' =
        moveIP' $ state { mp = if isMemEdge then memRight mp else memLeft mp }
    |
        -- copies the value of left neighbor into the current edge if the current edge is zero or negative
        -- and the value of the right neighbor if it's positive.
        c == '&' = do
            let neighbor = if isMemEdge then memRight mp else memLeft mp
            moveIP' . setCurMemEdge $ getMemEdge state neighbor
    |
        {------------------------- Miscellaneous -------------------------}
        -- terminates the program.
        c == '@' = moveIP' $ state { terminated = True, errorMessage = "@" }
    |
        -- '.' is a no-op: the IP will simply pass through.
        c == '.' = moveIP' state

    | otherwise = moveIP' . setCurMemEdge . fromIntegral . C.ord $ c
  where
    memEdge = getMemEdge state mp

    isMemEdge = memEdge > 0

    setCurMemEdge = setMemEdge state mp

    moveIP' = return . moveIP size

loadProgram :: String -> Program
loadProgram s =
    let instructions = foldr (\(q, r, x) -> Map.insert (q, r) x) Map.empty
            . concat $ populate s'
    in
        Program { instructions = instructions, state = state, size2 = size }
  where
    s' = lines . filter (/= ' ') $ formatProgram s

    size :: Integer
    size = fromIntegral . length . head $ s'

    ssize :: Integer
    ssize = size - 1

    state :: State
    state =
        State { memory       = Map.empty
              , ip           =
                    HexCoordinate { q = 0, r = -ssize, direction = East }
              , ipIdx        = 0
              , ips          =
                    A.listArray (0, 5)
                                [ HexCoordinate { q         = 0
                                                , r         = -ssize
                                                , direction = East
                                                }
                                , HexCoordinate { q         = ssize
                                                , r         = -ssize
                                                , direction = SouthEast
                                                }
                                , HexCoordinate { q         = ssize
                                                , r         = 0
                                                , direction = SouthWest
                                                }
                                , HexCoordinate { q         = 0
                                                , r         = ssize
                                                , direction = West
                                                }
                                , HexCoordinate { q         = -ssize
                                                , r         = ssize
                                                , direction = NorthWest
                                                }
                                , HexCoordinate { q         = -ssize
                                                , r         = 0
                                                , direction = NorthEast
                                                }
                                ]
              , mp           = HexCoordinate { q = 0, r = 0, direction = East }
              , terminated   = False
              , errorMessage = "Quit unexpectedly"
              }

    populate :: [String] -> [[(Integer, Integer, Char)]]
    populate =
        zipWith (\r l -> zipWith (\q x -> (q, r, x))
                                 [ (max (-r - (size - 1)) (-size + 1)) .. ]
                                 l)
                [ -(size - 1) .. ]

run :: Program -> IO ()
run program@Program{ instructions = instructions
                   , state =
                     state@State{ ip = ip@HexCoordinate{ q = q
                                                       , r = r
                                                       , direction = direction
                                                       }
                                , ipIdx = ipIdx
                                , terminated = terminated
                                , errorMessage = errorMessage
                                }
                   , size2 = size
                   }
    | terminated = do
        putStrLn ""
        when (errorMessage /= "@") $ hPutStr stderr errorMessage
    | otherwise = do
        let instruction = M.fromJust $ Map.lookup (q, r) instructions
        -- printf "\t\tip{q=%d, r=%d, direction=%s} ipIdx=%d\n"
        --        q
        --        r
        --        (show direction)
        --        ipIdx
        state <- execute state size instruction

        run $ program { state = state }

main :: IO ()
main = do
    args <- getArgs
    s <- readFile (head args)
    let program = loadProgram s
    run program