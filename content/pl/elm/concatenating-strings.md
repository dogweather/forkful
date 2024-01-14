---
title:                "Elm: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu czasami musimy połączyć dwa lub więcej ciągów znaków, aby uzyskać jedną dłuższą linię tekstu. W tym przypadku, konkatenacja jest koniecznością.

## Jak to zrobić

Możemy użyć operatora `++` aby połączyć (oraz ewentualnie przerwać) ciągi znaków. Na przykład:

```Elm
"Hello " ++ "world"
```

Output:
```
Hello world
```

Jeśli chcemy połączyć więcej niż dwa ciągi znaków, musimy użyć funkcji `append` i podać jej listę ciągów. Na przykład:

```Elm
append ["Hello", " ", "world"]
```

Output:
```
Hello world
```

## Głębsza analiza

Kiedy używamy operatora `++` lub funkcji `append`, interesujące rzeczy mogą się dziać pod spodem. W rzeczywistości, w tle, jest wykonywane wiele operacji połączeniowych, aby uzyskać oczekiwaną wartość. Dlatego, gdy używamy konkatencji w naszym kodzie, musimy pamiętać o wydajności i potencjalnych problemach związanych z wykorzystaniem dużej ilości połączeń.

## Zobacz również

- [Dokumentacja Elm o połączeniach](https://package.elm-lang.org/packages/elm/core/latest/String#++)

- [Przykłady konkatencji w Elm](https://github.com/elm-lang/elm-examples/tree/master/basics/strings)

- [Poradnik o wydajności w Elm](https://dev.to/mrinkazuki/performance-tips-for-elm-beginners-1i0l)