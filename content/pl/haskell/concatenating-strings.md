---
title:                "Haskell: Lączenie ciągów znaków"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie! W dzisiejszym poście zobaczymy, dlaczego tak ważną z funkcji w Haskellu jest łączenie stringów. Jeśli jesteś programistą i zastanawiasz się, dlaczego warto się tego nauczyć, to ten artykuł jest dla Ciebie!

## Jak to zrobić

Jedną z najbardziej podstawowych funkcji w Haskellu jest concat, która służy do łączenia różnych stringów w jeden. Możemy to zrobić za pomocą operatora ++ lub funkcji concat. Przykładowy kod i jego wyjście wyglądać będzie następująco:

```Haskell
-- Przykładowy kod łączący stringi
-- Operator ++
"Hello " ++ "world"
-> "Hello world"

-- Funkcja concat
concat ["Hello", " ", "world"]
-> "Hello world"
```

W przypadku łączenia większej liczby stringów, możemy wykorzystać funkcję concat z listą stringów jako argumentem, lub też zastosować funkcję join z biblioteki Data.List. Przykładowy kod i jego wyjście przedstawione jest poniżej:

```Haskell
-- Wykorzystanie funkcji concat
concat ["Hello", " ", "world", "!"]
-> "Hello world!"

-- Wykorzystanie funkcji join
import Data.List (join)
join ", " ["Hello", "world", "!"]
-> "Hello, world, !"
```

Na koniec warto wspomnieć o funkcji show, która pozwala na łączenie stringów z innymi danymi, jak na przykład liczbami. Dzięki temu możemy wyświetlać teksty i zmienne w jednej linii. Przykładowy kod i jego wyjście wyglądać będzie tak:

```Haskell
-- Wykorzystanie funkcji show
"Hello " ++ show 2020 ++ "!"
-> "Hello 2020!"
```

## Wnikliwe spojrzenie

W Haskellu istnieje wiele sposobów na łączenie stringów, jednak warto zwrócić uwagę, że nie jest to operacja zoptymalizowana pod względem wydajności. Dzieje się tak dlatego, że stringi w Haskellu są listami znaków, a listy w tym języku są leniwe, co oznacza, że niektóre operacje nie są wykonywane do momentu,aż dane są faktycznie potrzebne. W przypadku łączenia stringów, operacja ta może być wykonywana wielokrotnie, co wpływa na wydajność programu.

## Zobacz także

- [Haskell concat function documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#g:18)
- [Haskell Data.List module documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html)
- [Haskell show function documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#g:5)