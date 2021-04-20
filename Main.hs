import Data.Char
import Data.Bits
import Data.List
import Hex
import Numeric

type Bit = String
type HexStr = String
type Base64 = String

asciiCharacters = ['A' .. 'Z'] ++ ['a' .. 'z']  ++ ['0' .. '9']

hexMap = zip (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']) ["0000", "0001", "0010", "0011", "0100", "0101",
                                                             "0110", "0111", "1000", "1001", "1010", "1011",
                                                             "1100", "1101", "1110", "1111", "1010", "1011",
                                                             "1100", "1101", "1110", "1111"]

base64Map = zip [0 .. 65] (asciiCharacters ++ ['+', '/'])

frequencyTable = zip (['a' .. 'z'] ++ ['A' .. 'Z']) (concat $ replicate 2 [0.17, 0.44, 0.52, 0.32, 0.28, 0.4, 0.16, 0.42, 0.73, 0.051, 0.086, 0.24, 0.38, 0.23, 0.76, 0.43,
                                                                           0.022, 0.28, 0.67, 1.6, 1.2, 0.082, 0.55, 0.0045, 0.076, 0.0045])

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just a) = a
unwrapMaybe _ = undefined

hexToBit :: HexStr -> Bit
hexToBit hex = concat $ map (\x -> unwrapMaybe $ lookup x hexMap) hex

splitOnChunks :: [a] -> Int -> [[a]]
splitOnChunks str n
  | length currentChunk  < n = [currentChunk]
  | otherwise = [currentChunk] ++ splitOnChunks (drop n str) n
    where currentChunk = take n str

hexToBase64 :: HexStr -> Base64
hexToBase64 hex = map (\x -> unwrapMaybe $ lookup x base64Map) $ map bitToInt $ splitOnChunks (hexToBit hex) 6

hexToInt :: HexStr -> Integer
hexToInt hex = bitToInt $ hexToBit hex

xorHexStr :: HexStr -> HexStr -> HexStr
xorHexStr x y = let hex_1 = hexToInt x
                    hex_2 = hexToInt y in
                  showHex (xor hex_1 hex_2) ""

bitToInt :: Bit -> Integer
bitToInt bit = helper (reverse bit) 0 0 where
  helper reversed_bits index res
    | index == size = res
    | reversed_bits !! index == '0' = helper reversed_bits (index + 1) res
    | otherwise = helper reversed_bits (index + 1) (res + 2 ^ index)
      where size = length bit

bruteforceByteXor :: HexStr -> [String]
bruteforceByteXor encrypted_str = map (\ch -> map (\x -> chr $ fromIntegral (xor (bitToInt x) (toInteger (ord ch))))
                                        $ splitBits $ hexToBit encrypted_str ) asciiCharacters

bruteforceSingleByteXor :: HexStr -> [(String, Char)]
bruteforceSingleByteXor encrypted_str = map (\ch -> (map (\x -> chr $ fromIntegral (xor (bitToInt x) (toInteger (ord ch))))
                                              $ splitBits $ hexToBit encrypted_str), ch) asciiCharacters

updateFrequencyTable :: Char -> [(Char, Int)] -> [(Char, Int)]
updateFrequencyTable chr (x:xs)
  | null xs = []
  | fst x == chr = (fst x, snd x + 1):xs
  | otherwise = x:updateFrequencyTable chr xs

scoreFrequencyTable :: [(Char, Int)] -> Int -> Int
scoreFrequencyTable table originalStrSize = sum $ map (\x -> let averageFrequency = lookup (fst x) frequencyTable in
                                                               if null averageFrequency
                                                                  then 0
                                                                else
                                                                 let percentRange = abs ((snd x) - unwrapMaybe (averageFrequency)) in
                                                                   if percentRange <= 5 then 3 else 0)
                                            $ map (\x -> (fst x, fromIntegral(snd x) / fromIntegral(originalStrSize))) table

rateCanditate :: HexStr -> Int
rateCanditate canditate = helper canditate 0 $ zip ['a' .. 'z'] $ replicate 26 0 where
  helper current_str score strFrequencyTable | null current_str = score  + scoreFrequencyTable strFrequencyTable (length canditate)
                                             | elem firstChar asciiCharacters = helper (tail current_str) (score + 1) $
                                                                                                updateFrequencyTable (toLower firstChar) strFrequencyTable
                                             | otherwise = helper (tail current_str) (score - 3) strFrequencyTable
        where firstChar = head current_str

rateCanditates :: [HexStr] -> [(HexStr, Int)]
rateCanditates canditates = map (\x -> (x, rateCanditate x)) canditates

sortCanditates :: [(HexStr, Int)] -> [(HexStr, Int)]
sortCanditates indexed_Canditates = sortBy (\(_, a) (_, b) -> (flip compare) a b) indexed_Canditates

readEncryptedFile :: FilePath -> IO [String]
readEncryptedFile fileName = words <$> readFile fileName

decryptFile fileName = do
  lines <- readEncryptedFile fileName
  return $ take 10 $ sortCanditates $ concat $ map (\x -> rateCanditates $ bruteforceByteXor x) lines

splitBits :: Bit -> [Bit]
splitBits bits = splitOnChunks bits 8

repeatingXor :: String -> String -> HexStr
repeatingXor plainText key = concat $ map (\x -> if x > 15 then showHex x "" else "0" ++ showHex x "") $ helper plainText 0  where
  helper current_txt index        | null current_txt = []
                                  | otherwise = (xor (ord (head current_txt)) $ ord $ key !! index):helper (tail current_txt) (mod (index + 1) $ length key)

bitAnd :: Bit -> Bit -> Bit
bitAnd x y | null x = ""
           | head x == head y = [head x] ++ bitAnd (tail x) (tail y)
           | otherwise = "0" ++ bitAnd (tail x) (tail y)

countCh :: String -> Char -> Int
countCh str ch | null str = 0
               | head str == ch = 1 + countCh (tail str) ch
               | otherwise = countCh (tail str) ch

editDistance :: String -> String -> Int
editDistance str_1 str_2 = let bit_1 = hexToBit $ hex str_1
                               bit_2 = hexToBit $ hex str_2 in
                        helper bit_1 bit_2
                where helper x y | null x = 0
                                 | not $ head x == head y = 1 + helper (tail x) (tail y)
                                 | otherwise = helper (tail x) (tail y)


decryptRepeatingXor fileName = do
  keysizes <- [2 .. 40]
  fileContents <- readFile fileName
  step_3 <- map (\keysize -> (keysize, (editDistance (take keysize fileContents) $ take keysize $ drop keysize fileContents) / keysize) keysizes)
  step_4 <- head $ sortBy (\x(_, a) (_, b) -> (flip compare) a b) step_3
  step_5 <- splitOnChunks fileContents step_4
  step_6 <- transpose step_5
  step_7 <- map (\x -> head $ sortCanditates $ rateCanditates $ bruteforceXor x) step_6
