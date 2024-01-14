---
title:                "Haskell: Wycinanie podciągów"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Extrakcja podciągów jest jedną z podstawowych operacji w programowaniu. Pozwala na wycinanie fragmentów tekstu, co może być przydatne w różnych zastosowaniach. Przeczytaj ten artykuł, aby dowiedzieć się dlaczego warto poznać tę technikę w języku Haskell.

## Jak to zrobić

Extrakcja podciągów w Haskell jest bardzo prosta dzięki funkcji `take` i `drop`, które przyjmują odpowiednio ilość elementów do wycięcia z początku i końca listy.

```Haskell
take 3 "abcdefg" -- zwraca "abc"
drop 2 "12345" -- zwraca "345"
```

Możemy też wykorzystać `take` i `drop` jako argumenty funkcji `splitAt`, aby podzielić listę na dwie części.

```Haskell
splitAt 4 "Haskellers" -- zwraca ("Hask", "ellers")
```

Aby wyciąć podciąg o konkretnej długości z dowolnego miejsca w tekście, możemy wykorzystać funkcję `substr` z biblioteki `Text`.

```Haskell
import Data.Text (substr)

substr 4 6 "Hello World!" -- zwraca "o Worl"
```

Jeśli potrzebujemy wyciąć podciąg o wybranym początku i końcu, możemy skorzystać z funkcji `dropWhile` i `takeWhile`.

```Haskell
takeWhile (/= 'c') "abcde" -- zwraca "ab"
dropWhile (/= 'c') "abcde" -- zwraca "cde"
```

## Głębszy zanurzenie

Operacje na listach w Haskell są wyjątkowo wydajne, dlatego funkcje `take` i `drop` oraz ich pochodne działają szybko nawet na bardzo długich listach. Wykorzystanie funkcji z biblioteki `Text` może być korzystne, gdy chcemy wyciąć podciąg z bardzo dużego tekstu.

Ponadto, funkcje te mogą być wykorzystywane w wielu różnych kontekstach. Na przykład, wykorzystując `take` i `drop`, możemy implementować tzw. "paginację" w aplikacjach internetowych, aby wyświetlać tylko określoną część zawartości na stronie.

## Zobacz też

- Dokumentacja funkcji `take` i `drop`: https://hackage.haskell.org/package/base/docs/Prelude.html#v:take
- Biblioteka `Text`: https://hackage.haskell.org/package/text/docs/Data-Text.html
- Przykładowa implementacja paginacji w aplikacji internetowej: https://github.com/jirutka/gitter-paginate/blob/master/src/Gitter/Paginate.hs