---
title:    "Python: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi tarkistaa löytyykö hakemisto

Monista syistä pystyt tarkistamaan onko hakemisto olemassa ennen sen käyttämistä. Yksi yleisimmistä syistä on varmistaa, että ohjelmassa käytettyä polkua tai tiedostoa on olemassa ennen kuin yrität sitä käyttää. Tämä auttaa välttämään virheitä ja ohjelman kaatumisia.

## Kuinka tarkistaa onko hakemisto olemassa

Tässä esimerkissä näytän kuinka voit tarkistaa onko hakemisto olemassa käyttämällä Pythonin `os.path`-moduulia ja sen `exists()`-funktiota.

```Python
import os

hakemisto = "/kansio"

if os.path.exists(hakemisto):
    print("Hakemisto on olemassa!")
else:
    print("Hakemistoa ei löytynyt.")
```

Tämä yksinkertainen esimerkki tarkistaa, onko `/kansio`-niminen hakemisto olemassa ja tulostaa sen mukaisen viestin.

## Syvemmälle hakemistojen tarkistamiseen

On tärkeää huomata, että kyseinen esimerkki tarkistaa vain hakemiston olemassaolon, ei sen sisältämien tiedostojen tai alihakemistojen olemassaoloa. Jos haluat tarkistaa kaikki hakemiston sisältämät tiedostot ja alihakemistot, voit käyttää `os.path`-moduulin `listdir()`- ja `isfile()`-funktioita.

```Python
import os

hakemisto = "/kansio"

if os.path.exists(hakemisto):
    # Tarkista hakemiston sisältö
    for tiedosto in os.listdir(hakemisto):
        if os.path.isfile(os.path.join(hakemisto, tiedosto)):
            print(tiedosto + " on tiedosto.")
        else:
            print(tiedosto + " on hakemisto.")
else:
    print("Hakemistoa ei löytynyt.")
```

Tämä esimerkki tulostaa kaikki `/kansio`-hakemiston sisältämät tiedostot ja alihakemistot ja ilmoittaa, onko kyseessä tiedosto vai hakemisto.

## Katso myös
[Python `os.path`-moduulin dokumentaatio](https://docs.python.org/3/library/os.path.html)
[Tutorialspointin opas Pythonin `os.path`-moduulin käyttöön](https://www.tutorialspoint.com/python/os_path_exists.htm)
[Real Pythonin opas hakemistojen käsittelyyn Pythonissa](https://realpython.com/working-with-files-in-python/)