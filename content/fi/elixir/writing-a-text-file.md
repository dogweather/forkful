---
title:    "Elixir: Tekstitiedoston kirjoittaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on olennainen osa ohjelmointia. Tekstimuotoisen tiedoston kirjoittaminen Elixirillä mahdollistaa ohjelman suorittamisen ja toiminnan tallentamisen pysyvästi. Tämä voi olla hyödyllistä esimerkiksi tietokannan ylläpitämisessä tai konfiguraatiotiedostojen luomisessa.

## Näin teet sen

Tallentaaksesi tekstiä tiedostoon käytä `File.write` funktiota antaen sille tiedoston nimen ja tekstisisällön parametreina.

```Elixir
File.write("tekstitiedosto.txt", "Tämä on kirjoitettu Elixirillä")
```

```Elixir
IO.puts File.read("tekstitiedosto.txt")
=> "Tämä on kirjoitettu Elixirillä"
```

## Syvällisempää tietoa

Tekstimuotoisen tiedoston kirjoittamisessa voi myös käyttää `IO.write` funktiota, joka antaa mahdollisuuden toimia binäärimuotoisen datan kanssa.

```Elixir
#luo uusi tiedosto ja kirjoittaa siihen binääridataa
IO.binwrite("binääritiedosto.txt", <<1, 2, 3, 4>>)

#lue tiedostosta binääridataa ja tulosta se
IO.binread("binääritiedosto.txt") |> IO.inspect

=> <<1, 2, 3, 4>>
```

## Katso myös

- [Elixirin dokumentaatio tiedostojen kirjoittamisesta](https://hexdocs.pm/elixir/File.html#write/3)
- [Markdown syntaksi](https://www.markdownguide.org/basic-syntax/)