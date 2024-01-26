---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Kirjoitamme tekstitiedostoja tallentaaksemme dataa. Ohjelmoijat tekevät tämän pysyvän säilytyksen, lokitietojen kirjaamisen tai konfiguraatioiden jakamisen vuoksi.

## How to - Kuinka toimia:
Gleamilla tiedoston kirjoittaminen:

```
import gleam/io
import gleam/erlang
import gleam/erlang/try
import gleam/bit_builder.{BitBuilder}

fn write_to_file(data: String, filename: String) -> Result(BitBuilder, String) {
  try result = io.open_for_writing(filename)
  try _ = io.write(result, data)
  io.close(result)
}

pub fn main() {
  try _ = write_to_file("Hello, Gleam!", "example.txt")
  "File written successfully"
}
```

Esimerkkituloste:

```
File written successfully
```

## Deep Dive - Syväsukellus:
Gleam on moderni, turvallinen kieli, joka kääntyy Erlangiin. Historiallisesti tiedoston kirjoittaminen on tehty Erlangissa `file`-moduulin avulla. Vaihtoehtoja ovat käyttää suoraan Erlangin funktioita tai ulkopuolisia kirjastoja. Gleamissa tiedoston kirjoitus on yksinkertaistettu ja se tarjoaa parempaa virheenkäsittelyä käyttämällä `Result` tyypin.

## See Also - Katso Myös:
- Gleam dokumentaatio: [https://gleam.run](https://gleam.run)
- Erlang `file`-moduuli: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Officiell Gleam GitHub: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Gleam how-to guides: [https://gleam.run/book/tour](https://gleam.run/book/tour)
