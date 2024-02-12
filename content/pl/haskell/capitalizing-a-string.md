---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- pl/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:26.277432-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Capitalizacja ciągu polega na przekształceniu pierwszej litery danego ciągu na wielką literę, przy jednoczesnym zapewnieniu, że reszta liter pozostanie mała. Programiści robią to w celu formatowania wyjść, przestrzegania poprawności gramatycznej w tekstach lub poprawy czytelności generowanych danych.

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
