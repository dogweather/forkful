---
title:                "Haskell: Zapisywane dużymi literami łańcucha"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

**Dlaczego**

Cześć czytelnicy! Dziś chcę podzielić się z Wami pewnym ciekawym zagadnieniem w języku Haskell - jak zamieniać pierwszą literę w ciągu znaków na wielką literę. Może nie wydaje się to niczym spektakularnym, ale czasem taki prosty zabieg może ułatwić nam pracę programistyczną lub usprawnić wygląd naszego kodu. Zapraszam do lektury!

**Jak to zrobić**

Zacznijmy od tego, że w Haskellu ciągi znaków są traktowane jako listy znaków. Dzięki temu mamy dostęp do wielu funkcji, które działają na listach i mogą nam pomóc w zamienianiu liter na duże. Jedną z takich funkcji jest `toUpper`, która zamienia znak na dużą literę.

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = [] // jeśli lista jest pusta, zwracamy pustą listę
capitalize (x:xs) = toUpper x : xs // zamieniamy pierwszy znak na dużą literę i dołączamy resztę listy 
```

Przykładowe wywołanie tej funkcji wyglądałoby tak:

```Haskell
capitalize "haskell" 
```

Output:
```Haskell
"Haskell"
```

Proste, prawda? A co jeśli chcemy zamienić na dużą nie tylko pierwszą literę, ale również każdą kolejną literę w ciągu? W takim przypadku możemy wykorzystać funkcję `map`, która wywoła daną funkcję na każdym elemencie listy.

```Haskell
import Data.Char (toUpper)

capitalizeAll :: String -> String
capitalizeAll = map toUpper // skrócony zapis funkcji
```

Przykładowe wywołanie:

```Haskell
capitalizeAll "haskell" 
```

Output:
```Haskell
"HASKELL"
```

**Deep dive**

Istnieją również inne metody zamieniania liter na duże w Haskellu. Jedną z nich jest użycie notacji `{}` wraz z funkcją `map`, która umożliwia wywoływanie wielu funkcji na raz.

```Haskell
import Data.Char (toUpper)

capitalizeAll :: String -> String
capitalizeAll = map $ toUpper . toUpper
```

Dodatkowo, jeśli chcemy mieć pewność, że nasze ciągi znaków są bezpieczne i nie zawierają znaków, których nie chcemy zamieniać, możemy wykorzystać funkcję `filter`, która wybierze tylko odpowiednie znaki do zmiany.

```Haskell
import Data.Char (toUpper, isLetter)

capitalizeSafe :: String -> String
capitalizeSafe = map toUpper . filter isLetter
```

Oczywiście to tylko jedne z wielu możliwości, jakie daje nam funkcjonalny język Haskell. Zachęcam do eksperymentowania i odkrywania nowych sposobów na ten proste, ale przydatne zadanie.

**Zobacz również**

- [Funkcja `toUpper` z pakietu *Data.Char*](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html#v:toUpper)
- [Funkcja `map` z pakietu *Data.List*](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:map)
- [Funkcja `filter` z pakietu *Data.List*](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:filter)