---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Python: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstitiedoston kirjoittaminen on yksinkertainen tapa tallentaa tietoa tietokoneella. Ohjelmoijat käyttävät sitä usein tiedon tallentamiseen tai käsittelemiseen Python-ohjelmassa.

## Miten tehdään:
Pythonilla tekstitiedoston kirjoittaminen on helppoa. Aloita avaamalla tiedosto käyttäen avainsanaa ```with```. Sitten kirjoitetaan tiedot käyttäen ```write```-funktiota. Lopuksi suljetaan tiedosto ```close```-funktiolla. Alla on esimerkki koodista ja sen tulosteesta.

```Python
with open("tekstitiedosto.txt", "w") as tiedosto:
    tiedosto.write("Tämä on esimerkki tekstitiedoston kirjoittamisesta.")
```
Tuloste: 
- Ei näytetä mitään, mutta tiedosto "tekstitiedosto.txt" luodaan ja sen sisältöksi tulee "Tämä on esimerkki tekstitiedoston kirjoittamisesta."

## Syventyminen:
Tekstitiedoston kirjoittaminen on ollut tärkeä osa ohjelmointia jo pitkään. Nykypäivänä on myös muita tapoja tallentaa ja käsitellä tietoa, kuten tietokantoja tai JSON-tiedostoja. Pythonilla on myös muita tapoja kirjoittaa tekstitiedostoja, kuten käyttämällä ```with open(...) as tiedosto```-rakennetta.

## Katso myös:
- [Pythonin dokumentaatio tiedostojen käsittelystä](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)