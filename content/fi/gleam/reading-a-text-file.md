---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lukeminen tekstitiedostosta on prosessi, jossa ohjelmisto lukee tietoja tekstitiedostosta. Ohjelmoijat tekevät sen usein, koska tiedostojen käsittely on olennainen osa ohjelmointia, auttaa tiedon tallentamisessa ja jakamisessa.

## Miten tehdään:

```Gleam
import gleam/otp/process
import gleam/file.{File, OpenMode}

fn read_file(path: String) -> Result(List(String), String) {
  let file = File.open(path, OpenMode.Read)
  case file {
     Ok(_) -> 
         let lines = file.unwrap().read_lines()
         case lines {
            Ok(lines) -> 
                Ok(lines)
            Error(err) ->
                Error(err)
         }
     Error(err) ->
         Error(err)
  }
}

fn main(argv) {
  case read_file("test.txt") {
     Ok(lines) ->
         process.display(lines)
     Error(err) ->
         process.display(err)
  }
}
```

## Syvempi sukellus:

Lukeminen tekstitiedostoista on ollut olennainen osa ohjelmointia sen alkuperästä lähtien. Se on yksinkertainen ja tehokas tapa tallettaa ja jakaa tietoa. Gleam tarjoaa `file` moduulin tiedostojen käsittelyyn, joka sisältää `open` ja `read_lines` toiminnot tiedostojen lukemiseksi.

Vaihtoehtoisesti voit käyttää `read` funktiota lukeaksesi koko tiedoston kerralla, tai `read_bytes` funktiota jos haluat lukea tiedoston tavuina.

Tiedoston lukemisessa on tärkeää käsittellä virheitä asianmukaisesti. Gleam:n `Result` tyyppi tarjoaa turvallisen tavan käsitellä virheitä.

## Myös katso:

[Gleam:n virallinen dokumentaatio](https://gleam.run/book/)

[`File` moduuli](https://hexdocs.pm/gleam_stdlib/gleam/file.html)

[Tiedostojen luku Gleam:lla](https://www.learn-gleam.dev/tutorials/how-to-read-files/)