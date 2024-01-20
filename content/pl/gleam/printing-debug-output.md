---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Drukowanie informacji diagnostycznych to proces rejestrowania informacji na temat działania programu dla jego analizy. Programiści robią to, aby zrozumieć i rozwiązać problemy z programem.

## Jak to zrobić:

W języku Gleam drukowanie informacji diagnostycznych można wykonać za pomocą funkcji `debug`. Oto przykład:

```Gleam
import gleam/io.{println, stdout}

fn main(args: List(String)) {
  let _ = stdout()
  |> println(args)
}
```

Wynikowa informacja wyjściowa:

```
["argument1", "argument2"]
```

## Wgłąb tematu

(1) Historycznie, drukowanie informacji diagnostycznych zawsze było kluczową techniką w programowaniu. Działa na każdym systemie operacyjnym i nie wymaga żadnych dodatkowych narzędzi czy bibliotek. 

(2) Alternatywy dla drukowania informacji diagnostycznych to tworzenie na dysku plików z dziennikami lub użycie debuggera. 

(3) W Gleam, `println` wykorzystuje funkcję `display` do konwersji wartości na ciągi znaków. W rezultacie wartości są zapisywane w reprezentacji, która jest użyteczna dla debugowania, ale nie zawsze jest czytelna dla człowieka. 

## Zobacz też:
