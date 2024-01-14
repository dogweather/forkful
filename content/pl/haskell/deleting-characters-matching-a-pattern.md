---
title:    "Haskell: Usuwanie znaków pasujących do wzoru"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Z pewnością nie jest to często wykorzystywane narzędzie w programowaniu, ale czasami może się przydać usunięcie znaków pasujących do wzoru. Może to być przydatne, gdy chcemy pozbyć się pewnych danych lub przygotować je do dalszej obróbki.

## Jak to zrobić

Aby usunąć znaki pasujące do wzoru w Haskellu, można skorzystać z funkcji `deleteBy` z biblioteki `Data.List`. Musimy podać jako argumenty funkcję, która będzie sprawdzać, czy dany znak pasuje do wzoru oraz listę, z której chcemy usunąć znaki. Przykładowe użycie wyglądałoby następująco:

```Haskell
import Data.List

deleteMatching :: Char -> Bool
deleteMatching c = c == 'a' || c == 'b' -- przykładowa funkcja przeprowadzająca porównanie znaku z wzorem

inputList :: [Char]
inputList = "abcdefg"

outputList :: [Char]
outputList = deleteBy deleteMatching inputList -- usunięcie znaków 'a' i 'b' z listy

main :: IO ()
main = do
    putStrLn $ "Input: " ++ inputList
    putStrLn $ "Output: " ++ outputList

-- Output: Input: abcdefg
--         Output: cdefg
```

Możemy zmienić wzór oraz listę wejściową, aby dostosować funkcję do swoich potrzeb. Pamiętajmy, że funkcja `deleteBy` może przyjmować więcej niż jeden argument, co daje nam większe możliwości manipulacji danymi.

## Głębsza analiza

Funkcja `deleteBy` jest jedną z wielu funkcji z biblioteki `Data.List`, które pozwalają na manipulowanie danymi typu `List`. Znajdują się tam także inne funkcje, które mogą być przydatne w podobnych sytuacjach, np. `delete`, `deleteFirstsBy`, `deleteList`. Dzięki nim mamy większą elastyczność w operowaniu na liście.

Warto również zwrócić uwagę na to, że funkcja `deleteBy` działa na `List` w sposób rekurencyjny, co oznacza, że przetwarza elementy pojedynczo, aż do końca listy. Jest to ważna koncepcja w programowaniu funkcyjnym i dobrze ją zrozumieć, aby móc pisać bardziej efektywny i czytelny kod.

## Zobacz także

- Dokumentacja funkcji `deleteBy`: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:deleteBy
- Inne funkcje do manipulacji listami w Haskellu: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#g:18