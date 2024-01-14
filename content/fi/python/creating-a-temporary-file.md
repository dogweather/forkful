---
title:    "Python: Väliaikaisen tiedoston luominen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary files eli väliaikaiset tiedostot ovat erittäin hyödyllisiä ohjelmoinnissa, sillä ne tarjoavat tilapäisen säilytyspaikan tiedoille, jotka eivät tarvitse pysyvää tallennusta. Niitä käytetään esimerkiksi väliaikaisen datan tallentamiseen, väliaikaisiin muutoksiin tiedostoihin tai tilapäiseen tallennukseen, jota ei tarvita ohjelman käytön jälkeen.

## Miten

Tempfile-moduuli tarjoaa Pythonissa luokan TemporaryFile, joka mahdollistaa helpon väliaikaisten tiedostojen luomisen ja käytön. Alla on esimerkkikoodi, joka näyttää, kuinka luodaan ja kirjoitetaan väliaikaiseen tiedostoon ja lopuksi tulostetaan sen sisältö.

```Python
import tempfile

with tempfile.TemporaryFile() as tmp:
  tmp.write(b"Tervetuloa lukemaan blogikirjoitusta!")
  tmp.seek(0)
  print(tmp.read().decode())
```

Tämä tulostaa: "Tervetuloa lukemaan blogikirjoitusta!"

## Syvällinen sukellus

TemporaryFile-luokassa on useita hyödyllisiä ominaisuuksia, kuten mahdollisuus määrittää tiedoston tila (käyttämällä vaihtoehtoa "w+"), alustamisesta vastaavan koodin suorittaminen (käyttämällä "suffix" ja "prefix" -parametreja) ja automaattinen poistaminen ohjelman sulkeutuessa (käyttämällä "delete=True" -parametria).

TemporaryFile-luokassa on myös mahdollista määrittää, mihin hakemistoon väliaikainen tiedosto tallennetaan. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan väliaikaisesti tallentaa tiedosto johonkin tiettyyn kansioon, kuten käyttäjän tilapäinen hakemistoon ("tempfile.gettempdir()" -funktiolla).

## Katso myös

- [Pythonin tempfile-moduulin dokumentaatio](https://docs.python.org/3/library/tempfile.html)
- [Tietoa väliaikaisten tiedostojen käytöstä Unixissa](https://www.geeksforgeeks.org/unix-temporary-files/)
- [Tutorial väliaikaisten tiedostojen käytöstä Pythonissa](https://betterprogramming.pub/handling-temporary-files-in-python-1f21f6bacd60)