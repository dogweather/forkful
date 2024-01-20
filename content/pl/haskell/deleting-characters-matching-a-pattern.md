---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca, to proces eliminacji konkretnych sekwencji znaków z ciągu. Programiści robią to, aby uporządkować dane, sprzątając powtarzające się lub niepotrzebne znaki.

## Jak to zrobić:
```Haskell
import Data.Char
import Data.List

usunZnaki :: Char -> String -> String
usunZnaki _ [] = []
usunZnaki x (y:ys)
  | x == y = usunZnaki x ys
  | otherwise = y : usunZnaki x ys

main = print(usunZnaki 'a' "ananas") 
```
Po uruchomieniu powyższego kodu, otrzymamy ciąg "nns", w którym wszystkie litery "a" zostały usunięte.

## Więcej szczegółów
Usuwania znaków pasujących do wzorca było nieodzownym elementem programowania od początku jego dziejów. Historia jest pełna alternatywnych metodyk, takich jak regex lub korzystanie z funkcji wbudowanych innych języków. Chociaż ten proces jest dość prosty w Haskellu, zrozumienie jego implementacji wymaga dobrych umiejętności z list i rekurencji, które są kluczowymi technikami w Haskellu.

## Zobacz także
1. Dokumentacja biblioteki Haskell 'Data.Char' : http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html
2. Dokumentacja biblioteki Haskell 'Data.List' : http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
3. Manual Haskell - https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/