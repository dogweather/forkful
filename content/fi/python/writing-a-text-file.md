---
title:                "Python: Tiedostotiedon kirjoittaminen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on tärkeä osa Python-ohjelmointia, sillä se mahdollistaa tiedon tallentamisen muistiin ja sen jakamisen muille. Tekstitiedoston kirjoittaminen on nopea ja helppo tapa tallentaa tietoa, jota voidaan käyttää myöhemmin ohjelmassa.

## Miten

Tekstitiedoston kirjoittaminen Pythonissa on yksinkertaista. Alla olevassa esimerkissä käytetään "with" -lauseketta, jonka avulla voimme avata tiedoston ja kirjoittaa siihen haluamamme sisällön. 

```Python
with open("tiedosto.txt", "w") as tiedosto:
    tiedosto.write("Tervetuloa ohjelmointiblogiin!")
```

Tämä yksinkertainen koodinpätkä avaa tiedoston nimeltä "tiedosto.txt" ja kirjoittaa siihen valitun lauseen. Huomaa, että käytämme "w" -parametria, joka tarkoittaa, että kirjoitamme tiedostoon uutta sisältöä. Jos haluat lisätä jo olemassa olevaan tiedostoon, voit käyttää "a" -parametria.

Voimme myös halutessamme käyttää "with" -lauseketta määrittämään tiedoston nimen.

```Python
nimi = "blogi.txt"

with open(nimi, "w") as tiedosto:
    tiedosto.write("Tämä on hyödyllistä tietoa blogistani!")
```
Tuloksena on uusi tiedosto nimeltä "blogi.txt", jossa on haluttu sisältö.

## Syvempi sukellus

Voimme myös lisätä uusia rivejä tiedostoon käyttämällä "\n" -merkkiä. Tämä auttaa pitämään tiedoston selkeämpänä ja helposti luettavana.

```Python
with open("tiedosto.txt", "w") as tiedosto:
    tiedosto.write("Tervetuloa ohjelmointiblogiin!\n")
    tiedosto.write("Tässä opit uusia ohjelmointitaitoja.")
```

Lopputuloksena olevassa tiedostossa on kaksi erillistä riviä, joista toinen sisältää kiinteän lauseen ja toinen muuttuvan muuttujan arvon.

## Katso myös

Tässä artikkelissa opit kirjoittamaan tekstitiedostoja Pythonissa. Jatka opiskelua ja tutustu muihin Pythonin tiedostokäsittelyyn liittyviin aiheisiin osoitteessa:

- [Pythonin viralliset dokumentaatiot](https://docs.python.org/3/library/io.html)
- [Real Pythonin artikkeli: "Pythonin tiedostokäsittely"](https://realpython.com/read-write-files-python/)
- [TechVidvanin opetusohjelma: "Python - Tiedoston käsittely"](https://techvidvan.com/tutorials/python-file-handling/)