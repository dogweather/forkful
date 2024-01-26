---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:03.767677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Czytanie argumentów wiersza poleceń to pobieranie danych od użytkownika, kiedy uruchamia on nasz program. Programiści wykorzystują je do konfiguracji programów bez potrzeby twardokodowania opcji albo do przetwarzania danych wprowadzanych przez użytkownika w czasie wykonania.

## How to (Jak to zrobić)
W Gleam korzystanie z argumentów wiersza poleceń jest proste. Oto przykład:

```gleam
import gleam/io
import gleam/list

pub fn main(args: List(String)) {
  case list.head(args) {
    Some(arg) ->
      io.println("Pierwszy argument to: " ++ arg)
    None ->
      io.println("Brak argumentów.")
  }
} 
```
Jeśli uruchomisz ten program z argumentem w Terminalu, zobaczysz coś takiego:

```
$ gleam run . --example
Pierwszy argument to: example
```

## Deep Dive (Wgłębienie się)
Argumenty wiersza poleceń są tak stare, jak same systemy operacyjne z interfejsem tekstowym. Pozwalają użytkownikom na interakcję z programami w elastyczny sposób.

Alternatywami dla argumentów wiersza poleceń są pliki konfiguracyjne, interaktywne CLI (Command Line Interface), czy GUI (Graphical User Interface).

Implementacja w Gleam jest prosta dzięki List modułowi, który pozwala na łatwą manipulację wejściem jako listą stringów. Każdy argument jest rozdzielony spacją i traktowany jako oddzielny element listy.

## See Also (Zobacz także)
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/command-line-arguments) - Aby dowiedzieć się więcej o argumentach wiersza poleceń w kontekście języka Erlang, na którym Gleam jest oparty.
