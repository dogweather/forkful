---
title:                "Een tekstbestand schrijven"
date:                  2024-01-28T22:12:34.283809-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand schrijven betekent data opslaan als tekst die mensen kunnen lezen. Programmeurs doen dit om output op te slaan, apps te configureren of gebeurtenissen te loggen.

## Hoe te:
Gleam biedt bestands-I/O via zijn standaardbibliotheek. Hier is hoe je naar een bestand schrijft:
```gleam
import gleam/io
import gleam/result

pub fn write_to_file(contents: String) -> Result(Nil, IOError) {
  result.then(
    io.open("output.txt", [io.Write]),
    fn(file) { io.write(file, contents) }
  )
}

pub fn main() {
  case write_to_file("Hallo, Gleam!") {
    Ok(_) -> io.print("Succesvol naar het bestand geschreven")
    Error(err) -> io.print(err)
  }
}
```
Als het succesvol is, zal je `output.txt` "Hallo, Gleam!" bevatten.

## Diepere Duik
Historisch gezien is bestandsbehandeling cruciaal voor langdurige data-opslag. De benadering van Gleam lijkt op die van Erlang, waarop het gebouwd is. Alternatieven omvatten databasesystemen of in-geheugenopslag voor tijdelijke data. De standaardbibliotheek van Gleam houdt een slanke API aan, met een voorkeur voor expliciete foutafhandeling met het `Result` type.

## Zie Ook
- [Erlangs Bestandsmodule](http://erlang.org/doc/man/file.html) voor een begrip van de lager-niveau operaties waar Gleam op abstracteert.
- [Bestanden schrijven in Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.create), voor een vergelijking met een andere systementaal.
