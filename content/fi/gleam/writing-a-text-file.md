---
title:                "Tiedostotiedoston kirjoittaminen"
html_title:           "Gleam: Tiedostotiedoston kirjoittaminen"
simple_title:         "Tiedostotiedoston kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa tekstitiedosto? Tekstitiedostojen kirjoittaminen on hyödyllinen taito, joka voi auttaa sinua tallentamaan ja jakamaan tietoa tietokoneen kanssa.

## Kuinka

Kirjoittaminen tekstitiedostoja on helppoa Gleamilla. Seuraavassa esimerkissä luodaan uusi tiedosto nimeltä "testi.txt" ja kirjoitetaan siihen muutama rivi tekstiä:

```Gleam
let tekstitiedosto = File.create("testi.txt")
File.write(tekstitiedosto, "Tämä on ensimmäinen rivi.")
File.write(tekstitiedosto, "Tämä on toinen rivi.")
File.close(tekstitiedosto)
```

Tämän jälkeen voit avata tiedoston ja nähdä, että sisältö on tallentunut onnistuneesti.

```Gleam
let lukija = File.open("testi.txt")
File.read_all(lukija)
```

Tämä tulostaa:

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
```

Voit myös vaihtoehtoisesti käyttää `File.append`, jos haluat lisätä tekstiä olemassa olevaan tiedostoon sen sijaan, että kirjoitat sen kokonaan uudelleen.

## Syventyvä sukellus

Tekstitiedostojen kirjoittamisessa on monia hienouksia, joita voi oppia kokeilemalla ja tutkimalla Gleam-dokumentaatiota. Jotkut hyödyllisistä toiminnoista ovat esimerkiksi `File.exists?`, joka tarkistaa, onko tiedosto jo olemassa, ja `File.delete`, joka poistaa tiedoston. Lisäksi voit tutkia muita tiedostoon liittyviä toimintoja, kuten `File.copy` ja `File.move`, jotka ovat hyödyllisiä, kun haluat siirtää tai kopioida tiedostoja tietokoneellasi.

## Katso myös

- [Gleam-dokumentaatio](https://gleam.run/documentation)
- [Tekstitiedostojen käsittely Gleamilla](https://gleam.run/documentation/input_and_output#files)