---
title:                "Elixir: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen lukeminen on tärkeä taito Elixir-ohjelmointikielessä, sillä se antaa mahdollisuuden käsittellä suuria määriä dataa ja tehdä monimutkaisia operaatioita tekstin avulla. Se on myös olennainen osa tiedonsiirtoa ja tiedostojen käsittelyä Elixir-sovelluksissa.

## Kuinka tehdä

Tekstitiedostojen lukeminen Elixirillä on yksinkertaista ja tehokasta. Voit käyttää sisäänrakennettua `File`-moduulia ja sen `read`-funktiota lukeaksesi tiedostoja. Yksinkertainen esimerkki olisi seuraava:

```
Elixir

file = File.read("tiedosto.txt")

IO.puts(file)
```

Tämä koodi lukee tekstitiedoston nimeltä "tiedosto.txt" ja tulostaa sen sisällön konsoliin. Voit myös antaa erilaisia parametreja `File.read`-funktiolle, esimerkiksi `{:line, 2, 5}` joka palauttaa 2 riviltä 5 riviä. Voit myös käyttää `File.stream!`-funktiota lukeaksesi ja käsitelläksesi suuria tiedostoja tehokkaasti.

## Syvemmälle

Tekstitiedostojen lukeminen ei rajoitu pelkästään `File`-moduulin käyttämiseen. Voit myös käyttää `Erlang`-moduulia `:file` lukemaan tiedostoja. Tämä antaa sinulle suuremman hallinnan tiedostojen lukemisessa ja antaa mahdollisuuden käyttää erilaisia algoritmeja ja käsittelyjä.

Lisäksi voit käyttää `Stream` ja `Enum`-moduuleja lukeaksesi tiedostojen sisältöä ja suorittamaan monimutkaisempia operaatioita, kuten suodattamista tai järjestämistä. Tämä antaa sinulle mahdollisuuden tehdä edistyneitä tekstinkäsittelytoimintoja Elixirillä.

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/File.html#read/1)
- [How to Read Files in Elixir](https://www.tutorialspoint.com/elixir/elixir_file_io.htm)
- [Reading text files with Elixir - A Simple Guide](https://gist.github.com/edykim/83c6a12eab048b04346f)