---
title:                "Zmiana ciągu na wielkie litery"
html_title:           "Elixir: Zmiana ciągu na wielkie litery"
simple_title:         "Zmiana ciągu na wielkie litery"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Czym jest kapitalizacja ciągu znaków i dlaczego programiści to robią?

Kapitalizacja ciągu znaków oznacza zmianę pierwszej litery ciągu na dużą literę, a pozostałych na małe litery. Programiści często wykonują tę operację, aby poprawić czytelność tekstu lub zachować spójność w wyświetlaniu danych.

## Jak to zrobić?

```Elixir
String.capitalize("hello world")
```
Output: "Hello world"

```Elixir
String.downcase("Elixir")
```
Output: "elixir"

## Wnikliwe spojrzenie

Kapitalizacja ciągu znaków jest popularnym zadaniem w programowaniu i istnieje wiele sposobów jego wykonania, w tym funkcja ```capitalize``` w Elixir oraz metoda ```upper``` w innych językach programowania. Dzięki kapitalizacji, dane są wyświetlane w jednolity sposób, co ułatwia ich przetwarzanie i analizę. W przeszłości, kiedy drukowane teksty były złożone z dużych liter, a maszyny do pisania miały tylko klawisze wielkich liter, kapitalizacja była niezbędnym krokiem w tworzeniu wyraźnych i estetycznych dokumentów.

## Zobacz też

https://hexdocs.pm/elixir/String.html#capitalize/1
https://elixir-lang.org/getting-started/case-cond-and-if.html#capitalizing-a-string