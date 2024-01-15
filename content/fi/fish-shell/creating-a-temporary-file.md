---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Fish Shell: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary-tiedoston luominen on hyödyllinen toiminto ohjelmointia tehtäessä, sillä se mahdollistaa tiedon tallentamisen ja käsittelyn ilman pysyvää muutosta alkuperäisessä tiedostossa. Näin ollen voit kokeilla erilaisia muutoksia ja testata niiden vaikutuksia ennen kuin teet pysyviä muutoksia tiedostoon.

## Miten

Fish Shell tarjoaa kätevän tavan luoda väliaikaisia tiedostoja. Voit käyttää ```mktemp``` komentoa luodaksesi yksilöllisen väliaikaisen tiedoston, joka tallentaa sisältönsä oletusarvoisesti ```/tmp``` hakemistoon.

```
Fish Shell käyttö: mktemp [-du] [-p hakemisto] [-t sijainti] [malli]
```

Tässä on esimerkki kuinka voit luoda väliaikaisen tiedoston, tallentaa siihen sisältöä ja lukea sen sisältöä:

```
mktemp >> /tmp/tilapainen.txt
echo "Tämä on väliaikainen tiedosto" >> /tmp/tilapainen.txt
cat /tmp/tilapainen.txt
```

Tulostus:
```
Tämä on väliaikainen tiedosto
```

Voit myös määrittää haluamasi tiedoston nimen, käyttämälle esimerkiksi komentoa ```mktemp -t tiedostonimi```.

## Syvenny

Fish Shellin ```mktemp```-komento perustuu Unixin ```mktemp```-komentoon ja tarjoaa saman toiminnallisuuden, mutta käyttäjäystävällisemmällä tavalla. Tämän komennon käyttö on hyödyllistä, kun haluat tehdä tilapäisiä muutoksia tiedostoihin ilman vaaraa pysyvien muutosten tekemisestä.

Tämä komento tukee myös vaihtoehtoja, kuten ```-d``` joka luo hakemiston sijasta tilapäisen tiedoston ja ```-u``` joka varmistaa, että tiedoston nimi on uniikki monissa prosesseissa.

## Katso myös

- Man-sivu: ```mktemp(1)```
- Fish Shellin dokumentaatio: [Creating temp files](https://fishshell.com/docs/3.1/cmds/mktemp.html)