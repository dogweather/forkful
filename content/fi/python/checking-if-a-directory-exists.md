---
title:                "Tarkista, onko hakemisto olemassa."
html_title:           "Python: Tarkista, onko hakemisto olemassa."
simple_title:         "Tarkista, onko hakemisto olemassa."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tiedoston olemassaolon tarkistaminen on yksinkertainen, mutta tärkeä osa ohjelmoinnin arkea. Se tarkoittaa, että ohjelma kysyy, onko tietyllä polulla sijaitseva kansio olemassa vai ei. Ohjelmoijat tekevät tätä, jotta he voivat varmistaa, että heidän koodinsa toimii oikein ja estää mahdolliset virheet.

## Miten:
Esimerkki koodi, joka tarkistaa, onko kansio nimeltään "projekti" olemassa:

```Python
import os
if os.path.exists("./projekti"):
    print("Kansio nimeltä 'projekti' on olemassa.")
else:
    print("Kansiota nimeltä 'projekti' ei löytynyt.")
```

#### Tulostus:
```
Kansio nimeltä 'projekti' on olemassa.
```

Toinen tapa tarkistaa tiedoston olemassaolo on käyttää ```os.path.isdir()``` -funktiota:

```Python
import os
if os.path.isdir("./projekti"):
    print("Tämä on kansio.")
else:
    print("Tämä ei ole kansio.")
```

#### Tulostus:
```
Tämä on kansio.
```

## Syvemmälle:
Ohjelmoijat ovat joutuneet tarkistamaan tiedoston olemassaolon jo vuosikymmenien ajan, mutta koodi on muuttunut hieman riippuen käytössä olevasta ohjelmointikielestä. Esimerkiksi C-kielessä käytetään funktiota ```fopen()``` tiedoston avaamiseen, ja sen palauttama arvo voi kertoa, onko tiedosto olemassa vai ei. Pythonissa sen sijaan voidaan käyttää ```os.path.exists()``` tai ```os.path.isdir()``` -funktioita.

On myös muita tapoja tarkistaa tiedoston olemassaolo, kuten käyttämällä ```try/except``` -lohkoa tai ```os.stat()``` -funktiota. Nämä vaihtoehdot ovat käteviä, kun halutaan tehdä jotain erilaista, jos tiedostoa ei löydy.

## Katso myös:
- [Python os.path -dokumentaatio](https://docs.python.org/3/library/os.path.html)
- [Os.path module ja tiedostojärjestelmä - Real Python](https://realpython.com/python-pathlib/)