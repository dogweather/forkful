---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie argumentów z linii poleceń to proces, dzięki któremu program może otrzymać dane wejściowe bezpośrednio podczas uruchamiania. Programiści używają tego mechanizmu do konfigurowania swoich aplikacji w czasie rzeczywistym.

## Jak to zrobić:

Przykłady kodowania i wyniki w blokach kodu Gleam:

```Gleam
import gleam/list
import gleam/string.{from}

pub fn print_args() {
  let args = sys.args()
  let message = String.from(args)
  io.println(message)
}
```

Gdy uruchomisz ten kod z argumentami z linii poleceń, zobaczysz coś takiego:

```Sh
$ gleam run my_program arg1 arg2 arg3
arg1, arg2, arg3
```

## Deep Dive

Czytanie argumentów z linii poleceń nie jest nowym konceptem. Został on zapoczątkowany w latach 70. XX wieku z narodzinami języków UNIX i C. W języku Gleam jest to składowa wbudowana w moduł `sys`.

Alternatywą do czytania argumentów z linii poleceń może być korzystanie z plików konfiguracyjnych. Ale ta druga technika wymaga zrozumienia, gdzie pliki te należy umieścić i jak je sformatować, co może być bardziej skomplikowane dla użytkownika.

Szczegóły implementacji Gleam mogą się różnić. Argumenty z linii poleceń są przechowywane w liście, dzięki czemu można je łatwo przefiltrować lub zmapować.

## Zobacz także:

Możesz znaleźć więcej informacji na ten temat w oficjalnych dokumentach Gleam: