

module Cipher where 
import Char 

-- Caeser Cipher
rotate :: Int -> Char -> Char
rotate n c 
    | isLower c     = chr (((ord c - 97) + n) `mod` 26 + 97)
    | isUpper c     = rotate n (toLower c)
    | otherwise     = c 

encrypt :: Int -> [Char] -> [Char]
encrypt n plaintext = map (rotate n) plaintext   

decrypt :: Int -> [Char] -> [Char]
decrypt n ciphertext = map (rotate (-n)) ciphertext 


