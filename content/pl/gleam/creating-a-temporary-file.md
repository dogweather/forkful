---
title:                "Tworzenie pliku tymczasowego."
html_title:           "Gleam: Tworzenie pliku tymczasowego."
simple_title:         "Tworzenie pliku tymczasowego."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowych plików jest powszechnym zadaniem dla programistów. Polega ono na tworzeniu plików, które są używane tylko przez krótką chwilę i nie są potrzebne dłużej. Programiści często używają tego mechanizmu do przechowywania danych tymczasowych, np. podczas przetwarzania dużych zestawów danych.

## Jak to zrobić:

```Gleam
import gleam/temp

let temp_file = temp.file(temp.FileOptions{})

IO.print("Nazwa tymczasowego pliku to: {}", temp_file.name)
```

Output:

`Nazwa tymczasowego pliku to: rLPT5l.tmp`

## Głębsze wiertła:

Tworzenie tymczasowych plików jest stosunkowo nowym konceptem w programowaniu. Poprzednio programiści musieli ręcznie zarządzać tworzeniem, używaniem i usuwaniem tymczasowych plików. Dzięki bibliotece temp w Gleam tworzenie i zarządzanie tymczasowymi plikami jest znacznie prostsze i bezpieczniejsze.

Alternatywą dla tworzenia tymczasowych plików jest przechowywanie danych w pamięci RAM. Jednak ta metoda może być niepraktyczna przy przetwarzaniu dużych zestawów danych, ponieważ zajmuje ona pamięć komputera i może spowodować jej wyczerpanie.

Implementacja tworzenia tymczasowych plików w Gleam jest oparta na standardowej bibliotece języka Erlang. Wykorzystuje ona unikalny identyfikator procesu, aby generować unikalne nazwy dla tworzonych plików.

## Zobacz także:

- [Gleam Docs - Temp](https://gleam.run/modules/temp/)
- [Erlang Docs - temp](http://erlang.org/doc/man/temp.html)