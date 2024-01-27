---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Cos'è e Perché?
Scrivere un file di testo è salvare dati in forma leggibile. I programmatori lo fanno per persistenza dei dati, configurazioni, e logging.

## How to: - Come fare:
```elixir
import gleam/io
import gleam/erlang

pub fn write_to_file(text: String) -> io.Result(Nil) {
    let result = erlang.open_file("example.txt", [write])
    case result {
      Ok(file) -> 
        io.write(file, text)
      Error(error) ->
        Error(error)
    }
}

pub fn main() {
  case write_to_file("Ciao, mondo!") {
    Ok(_) -> io.println("File scritto con successo.")
    Error(err) -> io.println("Errore nella scrittura del file: ", err)
  }
}
```
Output:
```
File scritto con successo.
```

## Deep Dive - Approfondimento
La scrittura di file è essenziale da quando i computer esistono. Alternative includono database e memorizzazione in cloud, ma i file testuali sono semplici e universali. Gleam usa la VM Erlang per I/O file, con operazioni atomistiche per assicurare l'integrità dei dati.

## See Also - Vedi Anche
- [Gleam Stdlib Docs](https://hexdocs.pm/gleam_stdlib/)
- [Erlang File Module](http://erlang.org/doc/man/file.html)
