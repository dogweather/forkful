---
title:                "Python: Tarkista onko hakemisto olemassa."
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Eräs yleinen ongelma, joka voi tulla vastaan Python-ohjelmoinnissa, on se, että haluat tarkistaa, onko jokin kansio olemassa vai ei. Tämä voi olla tarpeellista esimerkiksi skannattaessa tiedostojärjestelmää tai tarkistaessa, pystyykö ohjelma suorittamaan tarvittavia toimintoja.

## Kuinka tehdä

Onneksi Pythonilla on sisäänrakennettu tapa tarkistaa, onko kansio olemassa. Tämä tapahtuu käyttämällä `os.path.exists()` -funktiota ja antamalla parametrina halutun kansion polku. Katso alla esimerkki, jossa tarkistetaan "tiedostot" -niminen kansio nykyisen työhakemiston sisältä:

```python
import os

if os.path.exists("tiedostot"):
    print("Kansio on olemassa!")
else:
    print("Kansiota ei löytynyt.")
```

**Tulostus:**

```python
Kansio on olemassa!
```

Tämä tarkistus palauttaa `True` tai `False` riippuen siitä, löytyykö annettu kansio vai ei. Voit myös käyttää `try`-`except` -lohkoa käsittelemään mahdollisia virheitä, kuten esimerkissä alla:

```python
import os

try:
    if os.path.exists("tiedostot"):
        print("Kansio on olemassa!")
    else:
        print("Kansiota ei löytynyt.")
except OSError:
    print("Kansiota ei pystytty tarkistamaan.")
```

## Syvemmälle

`os.path.exists()` -funktio toimii tarkistamalla, onko parametrina annetussa polussa oleva tiedosto tai kansio olemassa. Se ei sen sijaan tarkista, onko kyseisellä polulla myös oikeutta päästä tiedostoon tai kansioon. Tämän vuoksi on hyvä käyttää myös muita tarkistuksia, kuten `os.path.isdir()` tai `os.access()` -funktioita, mikäli tarvitaan tarkempaa tietoa tiedoston tai kansion ominaisuuksista.

## Katso myös

- Python `os` -dokumentaatio (https://docs.python.org/3/library/os.html)
- "Tiedostojen tarkastelu ja manipulointi Pythonilla" (https://realpython.com/working-with-files-in-python/)