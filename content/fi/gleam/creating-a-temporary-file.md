---
title:                "Tilapäisen tiedostopohjan luominen"
html_title:           "Gleam: Tilapäisen tiedostopohjan luominen"
simple_title:         "Tilapäisen tiedostopohjan luominen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Luodaan väliaikaisia tiedostoja ohjelmointiprosessissa. Tällä tavoin voidaan hallita muistinkäyttöä ja välttää tietoturvariskejä. Ohjelmoinnin aikana väliaikaisia tiedostoja käytetään myös tilapäisten tietojen tallentamiseen tai muokkaamiseen.

## Näin teet sen:
Tässä on yksinkertainen esimerkki väliaikaisen tiedoston luomisesta Gleamissa:

```Gleam
// Lataa tarvittavat moduulit
import gleam/io
import gleam/file

// Luo väliaikainen tiedosto nimeltä "tempfile"
tempfile = file.temp("tempfile")

// Avaa tiedosto kirjoittamista varten
file.write(tempfile, "Tämä on väliaikainen tiedosto.")

// Lue tiedoston sisältö ja tulosta se
gleam/io.println(file.read(tempfile))

// Sulje tiedosto ja poista se
file.close(tempfile)
file.delete(tempfile)

//Tulostaa:
//Tämä on väliaikainen tiedosto.
```

## Syvyyteen:
Väliaikaisten tiedostojen käyttö on yleinen käytäntö ohjelmoinnissa. Tiedostoja käytetään usein väliaikaisesti tietojen tallentamiseen tai väliaikaisena vaiheena tiedosto-toimintojen suorittamisessa. Alternatiivina Gleamin file.moduulille on myös mahdollista luoda ja käsitellä väliaikaisia tiedostoja manuaalisesti. Tämä voidaan tehdä käyttämällä POSIX-kirjaston ja C-koodia.

## Katso myös:
- Gleamin file.moduulin dokumentaatio: https://gleam.run/modules/gleam/file/latest/
- Gleam-kirjaston dokumentaatio POSIX: https://gleam.run/modules/gleam/posix/latest/
- Mahdollisuuden hallita tiedostoja suoraan C-koodissa: https://en.cppreference.com/w/c/io/tmpfile