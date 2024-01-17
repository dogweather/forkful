---
title:                "Wyodrębnianie podciągów"
html_title:           "Elixir: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Wyciąganie podciągów (substrings) to operacja, która polega na pobraniu fragmentu tekstu z danego napisu. Programiści często wykorzystują tę funkcjonalność do odczytywania danych z różnych źródeł i manipulowania nimi.

## Jak to zrobić:

Najprostszym sposobem na uzyskanie podciągu jest użycie funkcji ```String.slice(string, start, end)```, która pobierze fragment tekstu od indeksu startowego do indeksu końcowego. Na przykład, jeśli mamy napis "Hello world!", to ```String.slice("Hello world!", 0, 5)``` zwróci napis "Hello".

Można także skorzystać z funkcji ```String.split(string, separator)``` aby podzielić napis na fragmenty przy użyciu danego separatora. Na przykład, jeśli mamy napis "Elixir jest super!" to ```String.split("Elixir jest super!", " ")``` zwróci listę napisów ["Elixir", "jest", "super!"].

## Wnikliwa analiza:

Wyciąganie podciągów jest jednym z podstawowych operacji przy obróbce i manipulacji tekstu w programowaniu. Jest to szczególnie przydatna funkcjonalność w języku Elixir, ponieważ ten język jest silnie oparty na napisach jako podstawowym typie danych.

Alternatywnym sposobem na wyciąganie podciągów jest użycie wyrażeń regularnych, które pozwalają na dokładną specyfikację wzorca poszukiwanego w tekście. W Elixirze można to zrobić przy użyciu modułu Regex.

Wyciąganie podciągów jest realizowane wewnątrz języka Elixir przy użyciu indeksów napisów, co pozwala na szybkie i wydajne operacje na tekście.

## Zobacz także:

[Dokumentacja Elixir - Operacje na napisach](https://hexdocs.pm/elixir/String.html)

[Dokumentacja Elixir - Wyrażenia regularne](https://hexdocs.pm/elixir/Regex.html)

[Tutorial Elixir - Podstawy napisów](https://elixir-lang.org/getting-started/basic-types.html#string-concatenation-and-interpolation)