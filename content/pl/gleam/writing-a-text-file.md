---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego to zapisywanie danych do pliku w czytelnej formie. Programiści robią to, by przechowywać konfiguracje, logi działania aplikacji lub wymieniać dane między systemami.

## Jak to zrobić:
```gleam
import gleam/io
import gleam/result

pub fn main() {
  result.unwrap(
    io.write_file("hello.txt", "Witaj, świecie!")
  )
}
```
Po uruchomieniu tego kodu plik `hello.txt` zawiera tekst: `Witaj, świecie!`.

## Dogłębna analiza:
Pisanie do pliku tekstowego ma długą historię, ewoluując od prostych poleceń systemowych do zaawansowanych bibliotek I/O. W Gleam, `io.write_file/2` jest prostym podejściem, ale alternatywy jak Streamy mogą lepiej pasować do dużych danych lub pracy asynchronicznej. Detale implementacji zależą od systemu operacyjnego; Gleam posługuje się niżej poziomowymi operacjami systemowymi.
