---
title:    "Elixir: Alaeditorien purkaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Elixir on dynaaminen ja tehokas ohjelmointikieli, joka on suunniteltu modernin web-sovelluskehityksen tarpeisiin. Yksi Elixirin ominaisuuksista on kyky käsitellä tekstiä ja merkkijonoja tehokkaasti. Tässä blogikirjoituksessa tarkastelemme, miksi substringsien (osamerkkijonojen) erottaminen merkkijonosta on hyödyllistä ja miten se voidaan tehdä Elixirillä.

## Kuinka

Substringsien erottaminen merkkijonosta Elixirillä voidaan tehdä monella eri tavalla. Ensimmäinen tapa on käyttää built-in `String.substr()`-funktiota, joka palauttaa määritellyn alkuindeksin ja pituuden perusteella osamerkkijonon alkuperäisestä merkkijonosta. Esimerkki:

```Elixir
original = "Tervetuloa Elixir-maailmaan!"
substring = String.substr(original, 10, 8) # "Elixir-m"
IO.puts substring # tulostaa "Elixir-m"
```

Toinen tapa on käyttää `String.split()`-funktiota, joka jakaa merkkijonon määritellyn erotinmerkin perusteella ja palauttaa merkkijonot listana. Esimerkki:

```Elixir
original = "Tervetuloa Elixir-maailmaan!"
substrings = String.split(original, "-") # ["Tervetuloa Elixir", "maailmaan!"]
IO.inspect substrings # tulostaa taulukon
```

Tämä lähestymistapa on hyödyllinen esimerkiksi silloin, kun halutaan erottaa esimerkiksi CSV-tiedostosta tietyn sarakkeen arvot.

## Deep Dive

Elixirin `String`-moduulilla on monia muitakin funktioita, jotka ovat hyödyllisiä substringsien erottamisessa. Esimerkiksi `String.codepoints()`-funktio palauttaa merkkijonon kaikki merkit listana unicode-koodipisteinä. Tämä mahdollistaa merkkijonon käsittelyn merkki kerrallaan. Lisäksi `String.slice()`-funktio voi olla hyödyllinen, kun halutaan hakea merkkijonosta useita osia indeksien avulla.

Tärkeää substringsien erottamisessa on myös merkkijonojen encoding. Elixirissä on käytössä Unicode UTF-8 encoding, joten merkkijonojen manipulointi tapahtuu automaattisesti oikein kaikilla eri kielillä.

## Katso myös

- [Elixir String-moduulin dokumentaatio](https://hexdocs.pm/elixir/String.html)
- [Substringsien erottaminen Pythonilla](https://realpython.com/python-strings/#slicing-strings)
- [Regexien käyttö merkkijonojen käsittelyssä](https://hexdocs.pm/elixir/Regex.html)