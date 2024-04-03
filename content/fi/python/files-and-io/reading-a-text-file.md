---
date: 2024-01-20 17:55:09.181652-07:00
description: "Lukeminen tarkoittaa tiedoston datan siirt\xE4mist\xE4 ohjelmasi k\xE4\
  ytt\xF6\xF6n. Ohjelmoijat lukevat tekstitiedostoja, koska niist\xE4 saa t\xE4rke\xE4\
  \xE4 tietoa \u2013 asetukset,\u2026"
lastmod: '2024-03-13T22:44:56.160949-06:00'
model: gpt-4-1106-preview
summary: "Lukeminen tarkoittaa tiedoston datan siirt\xE4mist\xE4 ohjelmasi k\xE4ytt\xF6\
  \xF6n."
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
