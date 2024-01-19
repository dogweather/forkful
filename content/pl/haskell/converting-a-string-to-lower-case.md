---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Zamiana łancucha znaków na małe litery to jedna z najczęściej używanych funkcji. Programiści stosują ją do ujednolicenia tekstu i zapewnienia poprawnej porównywalności ciągów, niezależnie od wielkości liter.

## Jak to zrobić:

W Haskellu, można użyć funkcji `map` wraz z `toLower` z modułu Data.Char aby zmienić wszystkie litery w ciągu na małe. Oto przykład:

```Haskell
import Data.Char (toLower)

toLowerCase :: String -> String
toLowerCase = map toLower
```

Użyjmy go:
```Haskell
main = print(toLowerCase "HeLLo, WOrLd!")
```

Wynik:
```Haskell
"hello, world!"
```

## Deep Dive

Przyjrzyjmy się bliżej temu zagadnieniu.

1. **Kontekst historyczny**: Konwersja ciągów do białej litery została wprowadzony w językach programowania w latach 60., razem z pierwszymi systemami zarządzania bazami danych, które potrzebowały niezawodnego sposobu porównywania ciągów.

2. **Alternatywa**: W Haskellu istnieje wiele alternatyw. Możemy na przykład użyć biblioteki Text, jest ona znacznie szybsza w operacjach na dużych ciągach znaków.
```Haskell
import qualified Data.Text as T

toLowerCase :: T.Text -> T.Text 
toLowerCase = T.toLower
```

3. **Szczegóły implementacji**: Funkcja `toLower` z modułu Data.Char korzysta z Unicode. To oznacza, że prawidłowo obsłuży litery accentowe, oraz inne niestandardowe znaki.

## Zobacz również

- Dokumentacja funkcji `toLower` na Hoogle: https://hoogle.haskell.org/?hoogle=toLower
- Dokumentacja na `toLower` z Data.Text: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html