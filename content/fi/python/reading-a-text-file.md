---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:55:09.181652-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lukeminen tarkoittaa tiedoston datan siirtämistä ohjelmasi käyttöön. Ohjelmoijat lukevat tekstitiedostoja, koska niistä saa tärkeää tietoa – asetukset, dataa, ohjeita.

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
