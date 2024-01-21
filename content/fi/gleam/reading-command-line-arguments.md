---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:03.573152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lukemalla komentoriviargumentteja, ohjelmasi ottaa syötteen suoraan käynnistyksen yhteydessä. Se automatisoi tehtäviä ja mukauttaa ohjelman käyttäytymistä ilman koodin muokkaamista.

## How to: (Kuinka tehdä:)
```gleam
import gleam/io
import gleam/os

pub fn main() {
  let args = os.args()
  match args {
    [] -> io.print("No arguments given.")
    [first, ..] -> io.print("First argument: " ++ first)
    _ -> io.print("Something went wrong.")
  }
}
```
Kun ajat tämän ohjelman, saat nähdä jotain tällaista:
```
$ my_program
No arguments given.

$ my_program hello
First argument: hello
```

## Deep Dive (Syväsukellus)
Komentoriviargumenttien lukemisen juuret ovat vanhoissa Unix-järjestelmissä. Vaihtoehtoisesti, voit käyttää ympäristömuuttujia tai konfiguraatiotiedostoja, mutta ne eivät ole yhtä suoraviivaisia käynnistyksen yhteydessä. Gleamissa os.args()-funktio palauttaa listan merkkijonoja, jotka sisältävät kaikki komentoriviargumentit. Ohjelmoinnissa on tärkeää käsitellä erilaiset skenaariot, kuten edellä mainittiin: ei argumentteja, ensimmäinen argumentti ja virhetilanteet.

## See Also (Katso myös)
- Command-line arguments in the Unix Programming FAQ: [Unix Programming FAQ](http://www.faqs.org/faqs/unix-faq/programmer/faq/)
- Best practices for CLI design: [CLI guidelines](https://clig.dev/)