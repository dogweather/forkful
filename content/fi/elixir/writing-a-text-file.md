---
title:                "Elixir: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on olennainen osa monien ohjelmoijien arkea Elixir-ohjelmointikielen käytön myötä. Tekstikenttien kirjoittaminen voi olla hyödyllistä esimerkiksi tiedostojen tallentamisessa tai tietokantojen käsittelyssä.

## Miten

Tekstitiedoston kirjoittaminen on melko yksinkertaista Elixirissä. Se voidaan tehdä käyttämällä `File.write/2` -funktiota, joka ottaa parametreinaan tiedoston nimen ja kirjoitettavan tekstin. Esimerkiksi:

```Elixir
File.write("tiedosto.txt", "Tämä on tekstiä, joka tallennetaan tiedostoon.")
```

Tämän koodin suorittamisen jälkeen, uusi tiedosto nimeltä "tiedosto.txt" luodaan Elixir-projektisi juurikansioon ja siihen tallennetaan annettu teksti.

Voit myös halutessasi luoda uuden tiedoston käyttämällä `File.open/2` -funktiota ja kirjoittaa siihen tekstiä käyttämällä `IO.write/2` -funktiota. Tämä antaa sinulle enemmän hallintaa tiedostoon kirjoittamisessa. Esimerkiksi:

```Elixir
File.open("uusi_tiedosto.txt", [:write], fn file ->
  IO.write(file, "Tämä on uusi tiedosto.")
end)
```

Tässä esimerkissä luomme uuden tiedoston nimeltä "uusi_tiedosto.txt" ja kirjoitamme siihen tekstin "Tämä on uusi tiedosto." Käyttämällä `File.open/2` -funktiota ja `IO.write/2` -funktiota, voit myös muokata kirjoitustilan asetuksia, kuten käyttää binäärialiasta tai asettaa tiedostoon kirjoitettava koodausjärjestelmä.

## Syvä sukellus

Elixirin mahdollistama `File` -moduuli tarjoaa useita muita hyödyllisiä toimintoja tiedostoja käsitteleviin tehtäviin. Näihin kuuluu muun muassa `File.cp/2`, joka mahdollistaa tiedoston kopioinnin, ja `File.ls/1`, joka listaa tiedoston sisältämät tiedostot ja kansiot.

Lisäksi, `File` -moduuli sisältää myös `IO.gets/2` ja `IO.read/2` -funktiot, joiden avulla voit lukea tiedostosta tietoa käyttäen esimerkiksi rivien lukumäärää tai merkkijonoa rajauksena. Täysi lista `File` -moduulissa saatavilla olevista funktioista löytyy [Elixirin virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/File.html).

## Katso myös

- [Elixirin virallinen dokumentaatio tiedostojen käsittelystä](https://hexdocs.pm/elixir/File.html)
- [Elixirin tiedostojen käsittelyn perusteet -opas](https://www.learnelixir.tv/elixir-tutorial/files/)