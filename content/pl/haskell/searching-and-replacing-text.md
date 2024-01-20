---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyszukiwanie i zamiana tekstu to proces lokalizowania i modifikowania określonego ciągu znaków w tekście. Programiści dokonują tego, przede wszystkim, aby pozbyć się niechcianych znaków lub zastąpić je innymi.

## Jak to zrobić?

W Haskellu, możemy skorzystać z funkcji `subRegex` z biblioteki `Text.Regex`, aby przeprowadzić wyszukiwanie i zastąpić text. Oto jak to zrobić:
```Haskell
import Text.Regex

replaceText :: String -> String -> String -> String
replaceText old new = subRegex (mkRegex old) new

main = print $ replaceText "Haskell" "Python" "I love Haskell!" -- Wyjście: "I love Python!"
```
Jako pierwszy argument wprowadzamy wartość, którą chcemy zastąpić, jako drugi - tekst, którym chcemy zastąpić, a trzeci to tekst, w którym wykonujemy operację.

## Dokładne zrozumienie

Haskell może nie być pierwszym językiem, który przychodzi na myśl, gdy myślimy o operacjach na tekście, ale jest on niezwykle potężny dzięki swoim funkcjom i bibliotekom. Przykładem jest funkcja `subRegex`, która wykorzystuje wyrażenia regularne do wyszukiwania i zamieniania tekstu.

Alternatywą jest wykorzystanie biblioteki `Data.Text`, która posiada funkcje `replace`. Ważnym jest, że nie korzysta ona z wyrażeń regularnych, ale może być pomocna w prostszych przypadkach.

```Haskell
import Data.Text as T

replaceText :: String -> String -> String -> String
replaceText old new txt  = T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack txt)

main = print $ replaceText "Haskell" "Python" "I love Haskell!" -- Wyjście: "I love Python!"
```
Chodź `Data.Text.replace` nie obsługuje wzorców, jest ona znacznie szybsza i efektywniejsza niż `subRegex`, szczególnie dla dużych tekstów.

## Zobacz także

- ["Data.Text Documentation"](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html): Dokumentacja biblioteki `Data.Text`.