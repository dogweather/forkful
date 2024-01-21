---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:54:26.072382-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Czytanie pliku tekstowego to proces wczytywania danych z pliku przechowywanego na dysku. Programiści robią to, aby manewrować informacjami zawartymi w plikach, które mogą posłużyć do analizy danych, konfiguracji systemu, czy wczytywania zasobów potrzebnych do działania aplikacji.

## How to:
Przykładowy kod w Gleam do otwierania i czytania pliku tekstowego:

```gleam
import gleam/io
import gleam/erlang
import gleam/result

pub fn read_file(file_name: String) -> result.Result(String, io.Error) {
  file_name
  |> erlang.iolist_to_binary
  |> io.open(_, file:read_mode)
  |> result.map(io.read(_, 100_000))  // Czytaj do 100 kb danych
  |> result.map(result.unwrap)
}

// Aby użyć powyższej funkcji:
pub fn example_usage() {
  let result = read_file("example.txt")
  case result {
    Ok(contents) -> io.println(contents)
    Error(e) -> io.println("There was an error: " ++ e)
  }
}
```

Przykładowe wyjście po uruchomieniu example_usage:
```
Hello, World!
```

## Deep Dive
Czytanie plików tekstowych jest podstawową operacją w większości języków programowania, datującą się jeszcze od czasów, kiedy pisanie i czytanie z taśm było codziennością. Alternatywą do czytania całych plików jest strumieniowanie danych, przydatne przy dużych plikach, aby uniknąć nadmiernego użycia pamięci. Wykorzystanie bibliotek wejścia/wyjścia Gleam, takich jak `gleam/io` i `gleam/erlang`, pozwala na efektywne operacje na plikach przy minimalnym narzucie abstrakcji. Wybór odpowiedniego trybu odczytu pliku (np. `file:read_mode`) jest kluczowy, by uzyskać optymalną wydajność.

## See Also
- Erlang's file module which Gleam's file handling relies on: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)