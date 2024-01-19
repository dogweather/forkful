---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Długość łańcucha to liczba znaków w danym łańcuchu. Programiści potrzebują tej informacji w różnych kontekstach, takich jak generowanie błędów walidacyjnych, przycinanie tekstu lub operacje na tekście.

## Jak zrobić?

Aby sprawdzić długość łańcucha w Elm, używamy funkcji `String.length`. Oto przykład:

```Elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Cześć"))
```

Gdy uruchomisz ten kod, wyświetli się "5" – liczba znaków w słowie "Cześć".

## W Głąb Tematu

Elm to język funkcyjny, który podziela wiele cech z Haskell. Warto zauważyć, że choć Elm ma wbudowaną funkcję `String.length`, pamiętać o tym, że ta funkcja działa poprawnie jedynie na łańcuchach reprezentujących poprawny tekst w formacie UTF-16. W przypadku niewłaściwych sekwencji, wynik może być nieprzewidywalny.

Alternatywnie, jeśli operujesz na listach znaków zamiast łańcuchach, możesz użyć funkcji `List.length`.

Szczegółami implementacji `String.length` nie musisz się przejmować - język Elm został zaprojektowany z myślą o bezpieczeństwie i łatwości obsługi, więc te niskopoziomowe kwestie są dla Ciebie ukryte.

## Zobacz Też

Jeżeli chcesz zgłębić temat operacji na łańcuchach w Elm, zapraszam do lektury następujących źródeł:

- Dokumentacja Elm: [String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Dokumentacja o listach w Elm: [List](https://package.elm-lang.org/packages/elm/core/latest/List)
- Przydatny artykuł na blogu: [Understanding Strings in Elm](https://elmprogramming.com/strings.html)