---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:54:08.621674-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lukemalla tekstitiedoston koodi voi käsitellä tiedon sisältöä. Ohjelmoijat tekevät tämän tiedon hakemiseen, muokkaamiseen ja tallentamiseen.

## How to: (Kuinka tehdä:) 
```gleam
import gleam/io
import gleam/erlang

pub fn main() {
  let result = erlang.read_file("esimerkki.txt")
  case result {
    Ok(contents) -> io.println(contents)
    Error(error) -> io.println("Unable to read file: " ++ error)
  }
}
```
Output:
```
Tässä on esimerkkitekstisi sisältö.
```

## Deep Dive (Sukellus syvälle)
Reading a text file isn't anything new – it's foundational in programming. Historians would point to the dawn of computing. Alternatives to file reading include databases or online APIs, but files are simple and often the right tool.

Gleam is built on Erlang's rock-solid foundation. It inherits file reading capabilities from Erlang's own `file` module. The `erlang.read_file/1` function we use is a direct bridge to Erlang.

Understanding *how* Gleam reads files requires knowing about the Beam VM and Erlang's process model. Each file operation potentially blocks a process, so it's best handled asynchronously. Gleam encapsulates this complexity neatly.

## See Also (Katso myös)
- Erlang `file` module documentation: [Erlang File Module](http://erlang.org/doc/man/file.html)