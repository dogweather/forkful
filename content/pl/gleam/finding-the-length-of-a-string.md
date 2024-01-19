---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Znajdowanie długości łańcucha to proces określania liczby znaków w danym ciągu tekstowym. Programiści robią to, aby obsługiwać, manipulować, sprawdzać dane wejściowe lub dla operacji skracania.

## Jak to zrobić:

W Gleamie znajdowanie długości łańcucha jest proste i intuicyjne. Używamy wbudowanej funkcji `size`, jak w poniższym przykładzie:

```Gleam
let message = "Witaj, Gleam!"
let length = message.size
```

Gdy teraz wydrukujesz zmienną `length`, wyjście będzie wyglądało tak:

```Gleam
io.println(length) // Wyjście: 13
```

## W głąb tematu

Gleam, mimo iż jest stosunkowo nowym językiem programowania, posiada mechanizmy dobrze ugruntowane w historii informatyki. Funkcja `size` jest na przykład zaimplementowana podobnie jak w innych językach pokrewnych do Erlanga.

Alternatywą do `size` jest przeglądanie łańcucha po jednym znaku naraz, ale jest to mniej efektywne. Metoda `size` jest zdecydowanie szybsza i bardziej bezpośrednia.

Ciekawostką jest fakt, że `size` działa nie tylko na łańcuchach, ale również na listach i innych strukturach danych. Oznacza to, że jest bardzo uniwersalna.

## Zobacz również

1. [Dokumentacja Gleam's String](https://hexdocs.pm/gleam_stdlib/gleam/string@v0.18.2.html) - zawiera pełną listę funkcji łańcuchowych dostępnych w Gleam.
2. [Erlang String Operations](http://erlang.org/doc/man/erlang.html#length-1) - dla zrozumienia, jak Gleam, będąc bazowany na Erlangu, implementuje operacje na łańcuchach.