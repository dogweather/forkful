---
aliases:
- /pl/haskell/searching-and-replacing-text/
date: 2024-01-20 17:58:09.251172-07:00
description: "Wyszukiwanie i zamiana tekstu to podstawa edycji: zmieniasz jedno przez\
  \ drugie, proste. Programi\u015Bci to robi\u0105, by szybko poprawia\u0107 kody\
  \ i aktualizowa\u0107 dane."
lastmod: 2024-02-18 23:08:49.639156
model: gpt-4-1106-preview
summary: "Wyszukiwanie i zamiana tekstu to podstawa edycji: zmieniasz jedno przez\
  \ drugie, proste. Programi\u015Bci to robi\u0105, by szybko poprawia\u0107 kody\
  \ i aktualizowa\u0107 dane."
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## What & Why?
Wyszukiwanie i zamiana tekstu to podstawa edycji: zmieniasz jedno przez drugie, proste. Programiści to robią, by szybko poprawiać kody i aktualizować dane.

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
