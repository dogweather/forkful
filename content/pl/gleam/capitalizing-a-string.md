---
title:                "Gleam: Zmiana wielkości litery łańcucha"
simple_title:         "Zmiana wielkości litery łańcucha"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszych programach potrzebujemy zmienić wielkość liter w tekście. Na przykład, może chcemy zmienić wszystkie litery na dużych lub na małe. W tym artykule przyjrzymy się w jaki sposób możemy użyć języka programowania Gleam do zastosowania tej operacji na stringach.

## Jak To Zrobić

Aby zmienić wielkość liter w tekście, możemy skorzystać z funkcji `String.capitalize` dostępnej w bibliotece standardowej języka Gleam. W poniższym przykładzie użyjemy tej funkcji na przykładzie zdania "witaj świecie".

``` Gleam
let zdanie = "witaj świecie"
let zmienione_zdanie = String.capitalize(zdanie)

// Wynik: "Witaj świecie"
```

Jak widać, funkcja `String.capitalize` zmienia pierwszą literę zdania na wielką, a pozostałe litery pozostają bez zmian. Aby zmienić wszystkie litery na duże, możemy użyć funkcji `String.uppercase` lub na małe funkcji `String.lowercase`.

## Deep Dive

Funkcje `String.capitalize`, `String.uppercase` i `String.lowercase` są często używane do formatowania tekstu w naszym programie. Jednak warto pamiętać, że są one zależne od ustawień lokalnych naszego systemu operacyjnego, co może mieć wpływ na wyjście. Dlatego ważne jest, aby upewnić się, że nasze dane są poprawnie ustawione w odpowiedniej postaci przed przeprowadzeniem operacji zmiany wielkości liter.

## Zobacz także

- Dokumentacja języka Gleam: https://gleam.run/
- Biblioteka standardowa języka Gleam: https://gleam.run/modules/ 
- Przewodnik po operacjach na stringach w języku Gleam: https://gleam.run/docs/guides/strings