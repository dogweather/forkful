---
title:                "Python: Tarkastetaan, onko hakemistoa olemassa"
simple_title:         "Tarkastetaan, onko hakemistoa olemassa"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa onko hakemistoa olemassa?

On tärkeää tietää, onko hakemistoa olemassa, jotta voidaan varmistaa ohjelman sujuva toiminta ja estää mahdolliset virheet. Tarkistamalla ensin, onko hakemisto olemassa, voidaan välttää turhia ongelmia.

## Kuinka tarkistaa olemassa oleva hakemisto Pythonilla

Tarkistamiseen on olemassa useita tapoja, mutta yksi kätevin ja yksinkertaisin tapa on käyttää `os` -kirjastoa ja sen `path.exists()` -funktiota. Tämä funktio tarkistaa, onko annettu polku olemassa ja palauttaa `True` tai `False` arvon sen perusteella. Katso alla olevaa koodiesimerkkiä:

```Python
import os

# määritetään polku hakemistoon, jota halutaan tarkistaa
polku = "/kansio/hakemisto"

# tarkistetaan onko polku olemassa ja tallennetaan palautettu arvo muuttujaan
onko_olemassa = os.path.exists(polku)

# tulostetaan tulos
print(onko_olemassa)
```

Tulostus:
```
True
```

## Syvällisempi perehtyminen hakemistojen tarkistamiseen

Vaikka `path.exists()` on kätevä ja helppokäyttöinen funktio, on hyvä tietää myös muita mahdollisia tapoja tarkistaa hakemistoja. Esimerkiksi `os.listdir()` -funktio palauttaa listan kaikista hakemiston tiedostoista ja kansioista, jotka voidaan sitten käydä läpi ja tarkistaa halutun hakemiston olemassaolo. Tämä voi olla hyödyllistä, jos halutaan tarkistaa hakemistoja, jotka eivät ole suoraan annetulla polulla.

Lisäksi, jos halutaan luoda uusi hakemisto, mikäli sitä ei ole, voidaan käyttää `os.makedirs()` -funktiota, joka luo tarvittaessa myös välipolkuja. Esimerkiksi:

```Python
import os

# määritetään polku uudelle hakemistolle
polku = "/uusi_hakemisto/alihakemisto"

# luodaan hakemisto ja välipolku tarvittaessa
os.makedirs(polku)
```

## Katso myös

- `os` -kirjaston virallinen dokumentaatio: https://docs.python.org/3/library/os.html
- `pathlib` -kirjasto, joka tarjoaa uudemman ja helpommin käytettävän tavan käsitellä polkuja: https://docs.python.org/3/library/pathlib.html
- Lisätietoa Python-ohjelmoinnista ja hakemistojen käsittelystä: https://realpython.com/working-with-files-in-python/