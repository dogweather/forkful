---
title:    "Elixir: Tekstitiedoston kirjoittaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

Elixir: Miksi ja miten kirjoittaa tekstitiedosto

## Miksi

Tekstitiedostojen kirjoittaminen on tärkeä osa Elixir-ohjelmoinnin oppimista. Se antaa meille mahdollisuuden tallentaa ja käsitellä tietoja pysyvästi ilman, että ne häviävät, kun ohjelman suoritus päättyy.

## Miten

Tekstitiedoston kirjoittaminen Elixirissä on helppoa ja vaivatonta. Käytämme siihen `File.write` funktiota ja annamme sille tiedoston nimen ja halutun sisällön. Alla on esimerkki:

```elixir
File.write("tiedosto.txt", "Tämä on tekstiä tiedostoon!")
```

Kun suoritamme tämän koodinpätkän, se luo uuden tiedoston nimeltä "tiedosto.txt" ja tallentaa siihen annetun tekstin. Voimme myös käyttää `File.write!` funktiota, joka heittää virheen, jos tiedoston kirjoittaminen ei onnistu.

Voimme myös helposti lisätä tekstiä jo olemassa olevaan tiedostoon käyttämällä `File.write` funktiota sekä `:append`-optiota. Esimerkiksi:

```elixir
File.write("tiedosto.txt", "Lisättyä tekstiä", [:append])
```

Tämä koodipätkä lisää "Lisättyä tekstiä" tiedoston loppuun sen sijaan, että korvaisi koko sisällön.

## Deep Dive

Tekstitiedoston kirjoittaminen Elixirissä perustuu `IO` moduulin `write` funktioon, jota `File` moduuli käyttää sisäisesti. Tämä funktio kirjoittaa merkkijonon annettuun tietovirtaan ja palauttaa `:ok` atomimme, jos kirjoitus onnistuu.

Lisäksi voimme jälleen käyttää `IO.write` funktiota yhdessä `File.open` funktion kanssa saadaksemme täyden kontrollin tiedoston kirjoittamiseen. Esimerkiksi:

```elixir
File.open("tiedosto.txt", [:write], fn file ->
  IO.write(file, "Ensimmäinen rivi")
  IO.write(file, "Toinen rivi")
  IO.write(file, "Kolmas rivi")
end)
```

Tämä koodinpätkä avaa tiedoston, kirjoittaa siihen kolme riviä käyttäen `IO.write` funktiota ja sulkee tiedoston, kun `fn` lohko suoritetaan loppuun.

## Katso myös
- [Elixirin virallinen dokumentaatio tiedostojen kirjoittamisesta](https://hexdocs.pm/elixir/File.html#write/3)
- [IO moduulin dokumentaatio](https://hexdocs.pm/elixir/IO.html)
- [Elixirin virallinen dokumentaatio tiedoston avaamisesta](https://hexdocs.pm/elixir/File.html#open/2)