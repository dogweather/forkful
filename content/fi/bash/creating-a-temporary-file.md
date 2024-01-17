---
title:                "Tilapäistiedoston luominen"
html_title:           "Bash: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Väliaikaistiedoston luominen on yksinkertaisesti tiedoston luomista, joka on tarkoitus poistaa tai korvata myöhemmin. Tämä voi olla hyödyllistä esimerkiksi tiedon tallentamisessa väliaikaisesti ennen kuin se siirretään pysyvään sijaintiin. Ohjelmoijat tekevät väliaikaistiedostoja helpottaakseen tiedonhallintaa ja estääkseen turhia pysyviä tiedostoja.

## Miten:
Esimerkkejä ja tulostuksia koodinpätkissä ```Bash ...```.

Luodaan väliaikainen tiedosto käyttäen mktemp-komentoa:

```Bash
tempfile=$(mktemp)
echo "Tämä on väliaikainen tiedosto." >> $tempfile
cat $tempfile
rm $tempfile
```

Tulostus:

```
Tämä on väliaikainen tiedosto.
```

## Syvä sukellus:
Väliaikaisten tiedostojen käyttö on ollut välttämätöntä jo kauan ennen tietokoneita. Ennen tietokoneiden keksimistä työläiset tekivät väliaikaisia kopioita kirjoittamalla tekstejä kopioissentekijälle, jos alkuperäinen teksti piti pysyä muuttumattomana.

Vaihtoehtoisia tapoja luoda väliaikaistiedostoja ovat muun muassa Unixin mkstemp ja System V:n tmpnam, joita Bashin mktemp-komento käyttää taustalla. Väliaikaisten tiedostojen luomiseen käytetään usein myös /tmp-hakemistoa, johon on yleensä oikeudet kaikilla käyttäjillä.

Mktemp-komento luo yksilöllisiä tiedostonimiä, jotta vältetään mahdolliset epäonnistumiset tiedostonimesamassa ja turvaudutaan mahdollisiin turvallisuusuhkiin.

## Katso myös:
- [Bashin mktemp man-sivu](https://www.gnu.org/software/bash/manual/html_node/Creating-Temporary-Files.html)
- [Unixin mkstemp man-sivu](https://www.freebsd.org/cgi/man.cgi?query=mkstemp&apropos=0&sektion=0&manpath=FreeBSD+8.2-RELEASE&format=html)
- [System V:n tmpnam man-sivu](https://nxmnpg.lemoda.net/3/tmpnam)