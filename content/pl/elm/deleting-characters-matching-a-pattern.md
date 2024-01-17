---
title:                "Usuwanie znaków pasujących do wzoru"
html_title:           "Elm: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Usuwanie znaków pasujących do wzorca jest jedną z popularnych operacji w programowaniu Elm. Programiści często używają tego narzędzia do czyszczenia tekstu lub danych wejściowych, usuwając niepotrzebne znaki lub formatowanie.

## Jak to zrobić:

```Elm
text = "Hej, to jest przykładowy tekst!"
cleanedText = String.filter (\char -> char /= '!' && char /= '?') text

Texte en Francais = "Ceci est un exemple de texte en français !"
texteNettoye = String.filter (\char -> char /= '!' && char /= '?') texteEnFrancais

output = "Hej, to jest przykładowy tekst"
output2 = "Ceci est un exemple de texte en français"
```

## Głębszy Zanurzenie:

Usuwanie znaków pasujących do wzorca jest możliwe dzięki funkcji `String.filter`. Historia tego narzędzia sięga lat 60., kiedy to używane było w języku BASIC. Alternatywami dla `String.filter` są funkcje `String.replace`, `String.trim`, `List.filter` i `List.map`. Implementacja `String.filter` opiera się na funkcji `List.filter` oraz znanej jako *list comprehension* - pozwala ona na stworzenie nowej listy na podstawie istniejącej, wybierając tylko te elementy, które spełniają określone kryteria. 

## Zobacz Później:

- [Dokumentacja Elm](https://elm-lang.org/docs)
- [Funkcje Standardowe String Elm](https://package.elm-lang.org/packages/elm-lang/core/latest/String#filter)
- [Porównanie funkcji String w Lamie i Elm](https://dev.to/kodumu/lambdas-string-manipulation-comparing-elm-vs-e91)