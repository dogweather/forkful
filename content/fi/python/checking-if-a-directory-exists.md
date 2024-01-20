---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Kotlin: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Tarkistetaan, Onko Hakemisto Olemassa Pythonissa

## Mikä & Miksi?

Tiedostohakemiston olemassaolon tarkistaminen Pythonissa tarkoittaa, onko määritelty hakemisto olemassa tiedostojärjestelmässä. Tällainen tarkistus on tarpeen, ettemme kohtaa virheitä, kun yritämme Sijoittaa (tai hakea) tietoa hakemistosta, joka ei ole olemassa.

## Kuinka Tehdä:

Alla on koodiesimerkki, joka tarkistaa, onko tietyllä polulla oleva hakemisto olemassa :

```python
import os

def tarkista_hakemisto(hakemisto):
    if os.path.isdir(hakemisto):
        print(f"Hakemisto {hakemisto} löytyy!")
    else:
        print(f"Hakemisto {hakemisto} ei ole olemassa.")

tarkista_hakemisto("/polku/hakemistoon")
```

Tämä tulostaa "Hakemisto /polku/hakemistoon löytyy!" tai "Hakemisto /polku/hakemistoon ei ole olemassa." riippuen siitä, onko hakemisto olemassa.

## Syvempi Sukellus

Pythonin `os.path.isdir` -toiminto, jota käytimme ylläolevassa koodissa, tuli saataville Python 1.5.2 -versiossa julkisesti. Sen suorittaa kirjaston sisäinen `stat` -toiminto, joka tarkistaa tiedostojärjestelmän määritellyn polun olemassaolon ja tyypin.

Vaihtoehtona voit käyttää `pathlib` -moduulia, joka tuli saataville Python 3.4:ssä ja tarjoaa objekti-orientoituneen tavan käsittellä tiedostopolkuja:

```python
from pathlib import Path

hakemisto = Path('/polku/hakemistoon')
if hakemisto.is_dir():
    print(f"Hakemisto {hakemisto} löytyy!")
else:
    print(f"Hakemisto {hakemisto} ei ole olemassa.")
```

Tämä tarjoaa saman toiminnallisuuden, mutta elegantimman syntaksin.

## Katso Myös 

Lisätietoja Pythonin kansio- ja tiedostokäsittelystä saat seuraavista lähteistä:

1. Pythonin virallinen ohjedokumentaatio: [os](https://docs.python.org/3/library/os.html) ja [pathlib](https://docs.python.org/3/library/pathlib.html)
2. Tutorial: [Python 101: File and Directory Access](https://www.blog.pythonlibrary.org/2016/05/24/python-101-working-with-file-system-path-and-directory-access/)