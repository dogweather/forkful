---
title:                "Elixir: Standard errorin kirjoittaminen"
simple_title:         "Standard errorin kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen (standard error) on tärkeä taito Elixir-ohjelmoinnissa. Se auttaa kehittäjiä tunnistamaan ja korjaamaan virheitä, jotka ilmenevät ohjelman suorittamisen aikana.

## Miten

Kirjoittaminen standardivirheeseen on yksinkertaista Elixir-ohjelmoinnissa. Voit käyttää `IO`-moduulia, jota käytetään tietojen tulostamiseen ohjelman suorituksen aikana. Voit kirjoittaa viestin standardivirheeseen käyttämällä `IO.puts/2`-funktiota, joka ottaa kaksi argumenttia: tietojen tulostettavan viestin ja virhetason.

```
Elixir puts("Tämä on virheellinen viesti", :error)
```

Tämä koodi tulostaa viestin "Tämä on virheellinen viesti" standardivirheeseen ja asettaa sen virhetasoksi `:error`. Voit myös käyttää muita virhetasoja, kuten `:debug`, `:warn` ja `:info`, riippuen siitä, kuinka tärkeä viesti on.

## Syvällisempi tarkastelu

Kun ohjelma suoritetaan, se lukee koodia ja tulostaa tietoja standardiulostuloon (standard output). Jos ohjelmassa ilmenee virheitä, ne lähetetään standardivirheeseen. Tämän avulla kehittäjät voivat selvittää, missä kohdassa koodia virhe tapahtui ja korjata sen.

Elixirin standardivirheen käyttö on tärkeä erityisesti silloin, kun käsitellään tietokantoja ja ulkoisia tiedostoja. Jos ohjelma ei pysty lukemaan tai tallentamaan tietoa, siitä lähetetään viesti standardivirheeseen, mikä auttaa kehittäjiä tunnistamaan ja korjaamaan ongelman.

## Katso myös

- [Elixir IO-moduuli](https://hexdocs.pm/elixir/IO.html)
- [IO.puts/2-funktio](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Standard output ja standard error](https://en.wikipedia.org/wiki/Standard_streams)