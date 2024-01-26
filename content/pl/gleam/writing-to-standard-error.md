---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Pisanie na standardowe wyjście błędu (stderr) pozwala oddzielić normalne dane wyjściowe programu od informacji o błędach. Programiści robią to, aby ułatwić debugowanie i monitoring aplikacji.

## How to:
W Gleam, używamy modułu `io` do pisania na stderr. Oto jak:

```gleam
import gleam/io

pub fn main() {
  io.println("To jest normalne wyjście.")
  io.eprintln("To jest wyjście błędu!")
}
```
Output:
```
To jest normalne wyjście.
To jest wyjście błędu!
```
## Deep Dive
Pisanie na stderr ma długą historię w programowaniu i jest standardem w wielu językach. Istnieją alternatywy, np. logi czy pliki, ale stderr jest szybkie i proste. Implementacja w Gleam korzysta z BEAM (maszyna wirtualna Erlanga), która efektywnie zarządza różnymi strumieniami wyjścia.

## See Also
- [Understanding Standard Output and Standard Error](https://en.wikipedia.org/wiki/Standard_streams)
- [The BEAM Book - A description of the Erlang Runtime System](https://github.com/happi/theBeamBook)
