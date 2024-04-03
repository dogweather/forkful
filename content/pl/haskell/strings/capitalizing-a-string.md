---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:26.277432-07:00
description: "Jak to zrobi\u0107: W Haskellu mo\u017Cesz zrobi\u0107 capitalizacj\u0119\
  \ ci\u0105gu u\u017Cywaj\u0105c standardowej biblioteki, bez potrzeby korzystania\
  \ z bibliotek stron trzecich."
lastmod: '2024-03-13T22:44:35.434583-06:00'
model: gpt-4-0125-preview
summary: "W Haskellu mo\u017Cesz zrobi\u0107 capitalizacj\u0119 ci\u0105gu u\u017C\
  ywaj\u0105c standardowej biblioteki, bez potrzeby korzystania z bibliotek stron\
  \ trzecich."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
W Haskellu możesz zrobić capitalizację ciągu używając standardowej biblioteki, bez potrzeby korzystania z bibliotek stron trzecich.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Przykładowe użycie:
main = putStrLn $ capitalize "hello world"
```

Wyjście:
```
Hello world
```

Dla bardziej skomplikowanych scenariuszy lub dla wygody, możesz chcieć użyć biblioteki stron trzecich, takiej jak `text`, która jest popularna ze względu na efektywną manipulację ciągami w Haskellu.

Najpierw musisz dodać `text` do zależności swojego projektu. Następnie możesz użyć jej funkcji do zrobienia capitalizacji ciągu jak poniżej:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Przykładowe użycie z biblioteką text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Wyjście:
```
Hello world
```

Oba te przykłady demonstrują proste, ale skuteczne sposoby na zrobienie capitalizacji ciągu w Haskellu, z lub bez użycia bibliotek stron trzecich.
