---
title:                "Zmiana wielkości ciągu znaków"
html_title:           "Haskell: Zmiana wielkości ciągu znaków"
simple_title:         "Zmiana wielkości ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja stringów to proces zamiany pierwszej litery w wyrazie na wielką. Programiści często stosują ten zabieg, aby poprawić czytelność i spójność kodu. 

## Jak to zrobić?
W języku Haskell istnieje wiele sposobów na kapitalizację stringów. Poniżej przedstawione są przykłady z wykorzystaniem funkcji `toUpper` oraz `capitalize`:

```Haskell
import Data.Char (toUpper)

capitalizedString :: String -> String
capitalizedString [] = []
capitalizedString (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

main = do 
    putStrLn $ capitalizedString "hello world"
    -- Output: "Hello world"
    
    putStrLn $ capitalize "hello world"
    -- Output: "Hello world"
```

## Głębsze wody
W niektórych językach programowania, na przykład w Java, istnieją wbudowane metody do kapitalizacji stringów. W Haskellu musimy wykorzystać funkcje z modułu `Data.Char`, ale dzięki temu mamy większą kontrolę nad tym, w jaki sposób chcemy kapitalizować wyrazy. Istnieją również biblioteki z narzędziami do formatowania tekstu, co może ułatwić proces kapitalizacji.

## Zobacz też
- [Dokumentacja funkcji `toUpper`](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:toUpper)
- [Moduł `Data.Char`](https://hackage.haskell.org/package/base/docs/Data-Char.html)