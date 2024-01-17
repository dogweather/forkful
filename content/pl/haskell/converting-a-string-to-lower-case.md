---
title:                "Zamiana ciągu znaków na małe litery"
html_title:           "Haskell: Zamiana ciągu znaków na małe litery"
simple_title:         "Zamiana ciągu znaków na małe litery"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwertowanie ciągu znaków (string) na małe litery jest jednym z podstawowych zadań, z którymi spotkasz się jako programista w języku Haskell. Jest to proces, w którym każda litera w ciągu jest zamieniana na jej małą wersję. Programiści często wykonują tę czynność, aby ułatwić sobie porównywanie i manipulację ciągami znaków.

## Jak to zrobić:
Możesz skonwertować ciąg na małe litery za pomocą funkcji `toLower` z pakietu `Data.Char`. Poniżej znajdują się przykładowe kody oraz wyjście dla dwóch popularnych sposobów:

```Haskell
import Data.Char (toLower)

let str = "Hello World!"

-- sposób 1: użycie mapowania funkcji `toLower` na każdym znaku
map toLower str
-- wyjście: "hello world!"

-- sposób 2: użycie funkcji `map` z wyrażeniem lambda
map (\x -> toLower x) str
-- wyjście: "hello world!"
```

## Głębszy zanurzenie:
Historia konwertowania ciągu na małe litery sięga czasów, kiedy większość komputerów i drukarek obsługiwała tylko wielkie litery. Dlatego wiele języków programowania, w tym C i Java, korzystało z funkcji `tolower` lub `toLowerCase` do konwersji.

Alternatywnym podejściem do zmiany wielkości liter jest użycie funkcji `toUpper`, która zamienia litery na ich wielkie wersje. Możesz również użyć funkcji `mapM` zamiast `map` do konwersji wielu ciągów na małe litery jednocześnie.

Implementacja funkcji `toLower` w języku Haskell jest dość prostym zadaniem - funkcja ta używa tylko tabeli ASCII do konwertowania znaków. Jednak w bardziej zaawansowanej implementacji, należy uwzględnić obsługę znaków unicode.

## Zobacz także:
Możesz dowiedzieć się więcej o konwertowaniu ciągów na inne formy, takie jak duże litery lub inne języki, w źródłach:
- [Dokumentacja funkcji `toLower`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower)
- [Inne funkcje z pakietu `Data.Char`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)