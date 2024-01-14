---
title:    "Elm: Wyszukiwanie i zamiana tekstu"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu w Elm, może zdarzyć się, że potrzebujemy bardzo szybko zmienić pewien fragment tekstu, na przykład nazwę zmiennej lub funkcji. W takich sytuacjach bardzo przydatna jest funkcja wyszukiwania i zastępowania tekstu.

## Jak to zrobić

Aby wyszukać i zastąpić tekst w Elm, możemy skorzystać z funkcji "replace" z modułu "String". Przykładowo, jeśli chcemy zmienić nazwę zmiennej "liczba" na "number", możemy to zrobić w następujący sposób:

```elm
replace "liczba" "number" "liczba = 5" 
-- zwróci "number = 5"
```

Funkcja "replace" przyjmuje trzy argumenty: szukaną frazę, frazę zastępczą oraz tekst, w którym chcemy przeprowadzić zmiany.

## Głębszy wgląd

W Elm możemy również stosować wyrażenia regularne do wyszukiwania i zastępowania tekstu. Możemy użyć funkcji "Regex.replace" z modułu "Regex", aby przeprowadzić bardziej zaawansowane zmiany w tekście. Na przykład, jeśli chcemy zamienić wszystkie wystąpienia słowa "kot" na "pies" w tekście, możemy to zrobić w ten sposób:

```elm
Regex.replace (Regex.fromRegex "kot") "pies" "Kot lubi mleko"
-- zwróci "pies lubi mleko"
```

Funkcja "Regex.fromRegex" zamienia wzorzec regularny na obiekt regex, który zostaje następnie przekazany do funkcji "Regex.replace" jako pierwszy argument.

## Zobacz również

- [Dokumentacja funkcji replace w Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Dokumentacja funkcji Regex.replace w Elm](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace)
- [Przykładowe wyrażenia regularne w Elm](https://dev.to/buntine/elm-regex-cheat-sheet-5621)