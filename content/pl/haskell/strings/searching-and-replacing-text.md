---
date: 2024-01-20 17:58:09.251172-07:00
description: "How to: Pod koniec lat 60. pojawi\u0142y si\u0119 pierwsze narz\u0119\
  dzia do przetwarzania tekstu, jak `sed`. W Haskellu mo\u017Cna to robi\u0107 elegancko,\
  \ korzystaj\u0105c z funkcji\u2026"
lastmod: '2024-04-05T22:37:44.137720-06:00'
model: gpt-4-1106-preview
summary: "Pod koniec lat 60. pojawi\u0142y si\u0119 pierwsze narz\u0119dzia do przetwarzania\
  \ tekstu, jak `sed`. W Haskellu mo\u017Cna to robi\u0107 elegancko, korzystaj\u0105\
  c z funkcji wy\u017Cszego rz\u0119du i leniwego przetwarzania. Alternatyw\u0105\
  \ jest regex, ale wzorce szukamyz `Data.Text` i `Data.ByteString` s\u0105 cz\u0119\
  sto szybsze. Immutability w Haskellu oznacza, \u017Ce ka\u017Cde \"zamiany\" to\
  \ tak naprawd\u0119 tworzenie nowego ci\u0105gu znak\xF3w."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to:
```Haskell
import Data.List (isInfixOf, intercalate)

searchReplace :: String -> String -> String -> String
searchReplace searchStr replaceStr text = 
    intercalate replaceStr . splitOn searchStr $ text

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] _ = error "Delimiter cannot be an empty list"
splitOn _ [] = [[]]
splitOn delim list
    | delim `isInfixOf` list = let (start, rest) = breakOn delim list
                               in start : splitOn delim (drop (length delim) rest)
    | otherwise = [list]

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn _ [] = ([], [])
breakOn delim list@(x:xs)
   | delim `isPrefixOf` list = ([], drop (length delim) list)
   | otherwise = let (ys, zs) = breakOn delim xs in (x:ys, zs)

main = putStrLn $ searchReplace "world" "Haskell" "hello world!"
```

Output:
```
hello Haskell!
```

## Deep Dive
Pod koniec lat 60. pojawiły się pierwsze narzędzia do przetwarzania tekstu, jak `sed`. W Haskellu można to robić elegancko, korzystając z funkcji wyższego rzędu i leniwego przetwarzania. Alternatywą jest regex, ale wzorce szukamyz `Data.Text` i `Data.ByteString` są często szybsze. Immutability w Haskellu oznacza, że każde "zamiany" to tak naprawdę tworzenie nowego ciągu znaków.

## See Also
- Hoogle dla `Data.List`: https://hoogle.haskell.org/?hoogle=Data.List
- Regex w Haskellu: https://hackage.haskell.org/package/regex-base
- Haskell `text` package: https://hackage.haskell.org/package/text
