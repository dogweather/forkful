---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston kirjoittaminen Elixirissä tarkoittaa tiedon tallentamista levylle. Se on olennaista, kun halutaan säilyttää tietoa ohjelman ajon yli tai vaihtaa dataa eri systeemien kesken.

## How to:
Kirjoita tiedosto:
```elixir
File.write!("hello.txt", "Hei maailma!")
```
Lue tiedosto:
```elixir
IO.puts(File.read!("hello.txt"))
# Output: Hei maailma!
```
Lisää rivejä tiedostoon:
```elixir
File.write!("hello.txt", "Tervetuloa Elixirin maailmaan!", [:append])
```

## Deep Dive
Elixir-koodin tiedostonkäsittely perustuu Erlangin :file-moduuliin. Vaihtoehtoja ovat esimerkiksi Stream-moduuli suurempien tiedostojen käsittelyyn tai erilaiset tietokantakirjastot pysyvään datanhallintaan. Suorituskyky ja virheenkäsittely ovat tärkeitä tekijöitä, joita pohditaan tiedostoja kirjoitettaessa.

## See Also
- Elixirin virallinen dokumentaatio tiedostonkäsittelyyn: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Erlangin :file-moduuli: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Stream-moduuli suurien tiedostojen käsittelyyn: [https://hexdocs.pm/elixir/Stream.html](https://hexdocs.pm/elixir/Stream.html)
