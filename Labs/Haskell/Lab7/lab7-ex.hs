{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MiniCrypto where

{-
  PP, Laboratorul 7

  Laboratorul presupune implementarea unei mini-biblioteci de primitive
  criptografice: cifrări flux, cifrări bazate pe substituție (Caesar și
  Vigenere).
-}

import Data.List
import Data.Word
import Data.Bits
import System.Random hiding (randoms)
import Test.Framework

{-
  Funcții auxiliare: conversie Char-Word
-}
charToWord :: Char -> Word8
charToWord = fromIntegral . fromEnum

wordToChar :: Word8 -> Char
wordToChar = toEnum . fromIntegral

{-
  1. (1p)
  Construiți funcția myCycle, care ia ca argument o listă și întoarce lista
  repetată la infinit. Ex: myCycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,..]

  Hint: Puteți defini funcția „point-free”, folosind funcții din cadrul
  modulului Data.List.
  http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html
-}

test_myCycle :: IO ()
test_myCycle = do
  assertEqual (take 1 $ myCycle xs) (take 1 $ myCycle xs)
  assertEqual (take 42 $ myCycle xs) (take 42 $ cycle xs)
  where
  xs = [1,2,3,4]

myCycle :: [a] -> [a]
myCycle = concat.repeat

{-
  2. (3p)
  Construiți o funcție care întoarce un șir infinit de numere pseudo-aleatoare,
  plecând de la o valoare „seed” întreagă. Tipul elementelor listei va fi Word8
  (numerele vor fi între 0 și 255). Folosiți funcțiile definite în modulul
  System.Random pentru a genera numere. Folosiți fromIntegral pentru a realiza
  conversii între tipuri numerice întregi.
  http://www.haskell.org/ghc/docs/6.12.2/html/libraries/random-1.0.0.2/System-Random.html

  Ex: > take 10 $ randoms 42
  [38,166,220,81,67,142,213,118,105,10]

  Hint: Folosiți-vă de mkStdGen, next și (eventual) alte funcții din
  System.Random. *Nu* este necesară folosirea de funcții impure (care întorc
  valori de tipul IO).

  Observație: Funcția va fi implementată obligatoriu „point-free” (nu va avea
  parametri expliciți), folosind funcționale, compoziție și clauze let/where.
-}

randoms :: Int -> [Word8]
randoms = map (fromIntegral.fst) . iterate (\x -> next (snd x)) .next.mkStdGen

{-
  3. (2.5p)
  Implementați funcția substCrypt, care primește o funcție de substituție și un
  șir de caractere și întoarce șirul de caractere criptat.

  Implementați funcția tableToFunc, care primește o listă de asocieri
  (caracter-clar, caracter-criptat) și întoarce o funcție de substituție.

  Observație: substCrypt va fi implementată obligatoriu „point-free” (nu va
  avea parametri expliciți), folosind funcționale și/sau clauze let/where.
-}

prop_identity :: String -> Bool
prop_identity s = substCrypt id s == s

test_subst :: IO ()
test_subst = do
  assertEqual (substCrypt f "abcd") "cake"
  assertEqual (substCrypt (tableToFunc rot13Table) str) cryptstr
  where
  f 'a' = 'c'
  f 'b' = 'a'
  f 'c' = 'k'
  f 'd' = 'e'
  str = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG"
  cryptstr = "GURDHVPXOEBJASBKWHZCFBIREGURYNMLQBT"

rot13Table = [('A','N'), ('B','O'), ('C','P'), ('D','Q'), ('E','R'),
              ('F','S'), ('G','T'), ('H','U'), ('I','V'), ('J','W'),
              ('K','X'), ('L','Y'), ('M','Z'), ('N','A'), ('O','B'),
              ('P','C'), ('Q','D'), ('R','E'), ('S','F'), ('T','G'),
              ('U','H'), ('V','I'), ('W','J'), ('X','K'), ('Y','L'),
              ('Z','M')]

tableToFunc :: [(Char, Char)] -> Char -> Char
tableToFunc t c = snd $ head (filter (\x -> (fst x) == c) t)

substCrypt :: (Char -> Char) -> String -> String
substCrypt = undefined

{-
  4. (2.5p)
  Implementați funcția xorCrypt. Pentru un șir de caractere și o cheie dată,
  funcția va întoarce o listă de Word8 reprezentând textul criptat cu XOR pe
  biți între cheie și text.

  Observație: Implementați funcția „point-free” (decât primul parametru al
  funcției va fi explicit).

  Observație: Se presupune că textul și cheia au aceeași lungime.

  Hint: Folosiți-vă de funcțiile de calcul pe biți din modulul Data.Bits.
  http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Bits.html
-}

test_xor :: IO ()
test_xor = assertEqual
  (xorCrypt "This is a super secret string!" $ take 30 $ repeat 2)
  [86,106,107,113,34,107,113,34,99,34,113,119,114,103,112,34,113,103,97,112,
   103,118,34,113,118,112,107,108,101,35]

prop_neutral_zero :: String -> Bool
prop_neutral_zero s = map charToWord s == xorCrypt s zeroes
  where
  zeroes = take (length s) $ repeat 0

prop_inverse :: String -> Bool
prop_inverse s = prop_inverse2 s k
  where
  k = take (length s) $ randoms 42

xorCrypt :: String -> [Word8] -> [Word8]
xorCrypt s = undefined

{-
  5. (1p)
  Implementați xorCrypt2, o variantă a funcției xorCrypt care poate primi chei
  de lungime arbitrară. Dacă lungimea cheii e mai mică decât cea a textului,
  atunci se va face wrap-around la cheie. Ex: cheia "abc" de lungime 5 va
  deveni "abcab".

  Observație: Este obligatoriu să reutilizați xorCrypt pentru implementarea lui
  xorCrypt2.
-}

prop_length2 :: String -> [Word8] -> Bool
prop_length2 s k = length k == 0 || length (xorCrypt2 s k) == length s

prop_inverse2 :: String -> [Word8] -> Bool
prop_inverse2 s k = length k == 0 ||
  xorCrypt2 (map wordToChar $ xorCrypt2 s k) k == map charToWord s

xorCrypt2 :: String -> [Word8] -> [Word8]
xorCrypt2 s k = undefined

{-
  6. (BONUS, 2p)
  Implementați algoritmul de criptare Vigenere:
  https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher

  Funcția vigenereCrypt primește un text și o cheie de lungime arbitrară și
  întoarce textul criptat cu cheia.

  Observație: Se presupune că textul și cheia sunt litere din alfabet
  lower-case.

  Hint: Puteți folosi substCrypt ca bază pentru criptare.

  Hint: Puteți folosi http://www.vigenere.net/ ca referință.
-}

test_vigenere :: IO ()
test_vigenere = assertEqual
  (vigenereCrypt "thecakeisalie" "glados")
  "zsefocktsdzak"

vigenereCrypt :: String -> String -> String
vigenereCrypt = undefined

runTests :: IO ()
runTests = htfMain htf_thisModulesTests
