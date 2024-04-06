---
date: 2024-01-20 17:55:09.181652-07:00
description: "How to: (Kuinka tehd\xE4:) Tiedostoja on luettu siit\xE4 l\xE4htien,\
  \ kun tietokoneet alkoivat k\xE4ytt\xE4\xE4 ulkoisia tallennusmedioita. Historiallisesti\
  \ tiedot luettiin\u2026"
lastmod: '2024-04-05T22:51:10.319684-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Tiedostoja on luettu siit\xE4 l\xE4htien, kun tietokoneet\
  \ alkoivat k\xE4ytt\xE4\xE4 ulkoisia tallennusmedioita."
title: Tekstitiedoston lukeminen
weight: 22
---

## How to: (Kuinka tehdä:)
```Python
# Tiedoston avaaminen ja lukeminen
with open('esimerkki.txt', 'r') as tiedosto:
    sisalto = tiedosto.read()
    print(sisalto)

# Tiedostosta rivien lukeminen
with open('esimerkki.txt', 'r') as tiedosto:
    for rivi in tiedosto:
        print(rivi.strip())
```
Sample output:
```
Hei maailma!
Tämä on tekstiriviesimerkki.
```

## Deep Dive (Sukellus syvemmälle)
Tiedostoja on luettu siitä lähtien, kun tietokoneet alkoivat käyttää ulkoisia tallennusmedioita. Historiallisesti tiedot luettiin puhdistetusta kortista tai teipiltä, mutta nykyisin yleisimmin levyltä tai SSD:ltä. Vaihtoehtoja `open()`-funktion käytölle ovat mm. `io`-moduulin luokat kuten `StringIO` testaukseen ja `BytesIO` binaaritiedostojen käsittelyyn. Varsinainen lukuprosessi tehdään buferoidusti, mikä tarkoittaa, että tiedot ladataan muistiin pienissä osissa tehokkuuden ja resurssienhallinnan kannalta.

## See Also (Katso myös)
- [Pythonin virallinen dokumentaatio tiedostonkäsittelystä](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Pythonin `io`-moduulin dokumentaatio](https://docs.python.org/3/library/io.html)
- Wikipedian artikkeli tiedostojärjestelmistä: [https://fi.wikipedia.org/wiki/Tiedostoj%C3%A4rjestelm%C3%A4](https://fi.wikipedia.org/wiki/Tiedostoj%C3%A4rjestelm%C3%A4)
