---
title:                "Zastosowanie wielkich liter w ciągu znaków"
html_title:           "Haskell: Zastosowanie wielkich liter w ciągu znaków"
simple_title:         "Zastosowanie wielkich liter w ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedykolwiek, dlaczego chciałbyś zmienić pierwszą literę słowa w zdaniu na wielką? Może chcesz poprawić gramatykę swojego tekstu lub po prostu nadać mu bardziej formalny wygląd. Bez względu na powód, kapitalizacja ciągów jest ważnym aspektem programowania w Haskellu.

## Jak to zrobić

Aby zmienić pierwszą literę w ciągu znaków na wielką, można użyć funkcji `toUpper` z modułu `Data.Char`. Na przykład:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = toUpper (head str) : tail str

-- Przykładowe wywołanie funkcji:
capitalize "haskell" -- zwróci "Haskell"
```
Możesz również wykorzystać wbudowaną w Haskell funkcję `capitalize` do kapitalizacji całego zdania bez konieczności dzielenia go na poszczególne wyrazy. Przykładowy kod może wyglądać tak:

```Haskell
import Data.Char (toUpper)

capitalizeSentence :: String -> String
capitalizeSentence str = unwords (map capitalize (words str))

-- Przykładowe wywołanie funkcji:
capitalizeSentence "haskell jest wspaniały językiem programowania" -- zwróci "Haskell Jest Wspaniały Językiem Programowania"
```

## Głębszy wgląd

Podczas korzystania z funkcji `toUpper` należy zwrócić uwagę na to, aby jej argument był typu `Char`, a nie `String`. Dlatego też wykorzystujemy funkcję `head`, która zwraca pierwszy element listy, a następnie używamy funkcji `tail`, aby utworzyć nową listę, która składa się z oryginalnych znaków poza pierwszym.

Wykorzystując funkcje `capitalize` i `capitalizeSentence`, możemy również zmienić tylko pierwszą literę zdania na wielką, a pozostałe litery zostaną zachowane w oryginalnej formie.

## Zobacz również

Jeśli jesteś zainteresowany innymi podstawowymi operacjami na ciągach znaków w Haskellu, możesz zapoznać się z następującymi linkami:

- [Oficjalna dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Wykorzystanie funkcji `map` w Haskellu](https://wiki.haskell.org/Map)
- [Przykłady wykorzystania wbudowanych funkcji do pracy z ciągami znaków](http://zvon.org/other/haskell/Outputtext/Strings_f.html)