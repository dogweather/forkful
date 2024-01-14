---
title:    "Python: Tarkistetaan löytyykö hakemistoa"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Kun kirjoitat Python-ohjelmia, saatat jossain vaiheessa joutua tarkistamaan, onko tietty hakemisto olemassa. Tämä on tärkeää esimerkiksi silloin, kun haluat tallentaa tiedostoja tiettyyn hakemistoon tai lukea sieltä tiedostoja. Tässä blogipostauksessa kerromme, miten voit tarkistaa hakemiston olemassaolon Pythonilla.

## Miten tehdä

Python tarjoaa valmiin toiminnon hakemiston tarkistamiseen. Tämä toiminto on nimeltään `os.path.exists()` ja se tarkistaa, onko annetussa polussa oleva tiedosto tai hakemisto olemassa.

```Python
import os

# Määritellään polku, jossa haluamme tarkistaa olemassaolon
polku = "/kansio/tiedosto.txt"

# Tarkistetaan polun olemassaolo ja tallennetaan tulos muuttujaan
tulos = os.path.exists(polku)

# Tulostetaan tulos
print(tulos)
```

Tämä esimerkki tulostaa `True`, jos tiedosto tai hakemisto löytyy annetusta polusta, ja `False`, jos se ei ole olemassa. Voit myös käyttää tätä toimintoa hakemistopolkujen lisäksi myös tiedostopoluissa.

```Python
import os

# Määritellään polku, jossa haluamme tarkistaa tiedoston tai hakemiston olemassaolon
polku = "/kansio/tiedosto.txt"

# Tarkistetaan polun olemassaolo ja tallennetaan tulos muuttujaan
tulos = os.path.exists(polku)

# Tulostetaan tulos
print(tulos)
```

Tämä koodi palauttaa saman tuloksen kuin ensimmäinen esimerkki, koska `os.path.exists()` tarkistaa sekä tiedostot että hakemistot.

## Syvällisempi sukellus

Jos haluat tarkemmin ymmärtää, miten `os.path.exists()` toimii, voit tutustua siihen, kuinka Python käsittelee tiedostoja ja hakemistoja.

Kun käytät `os.path.exists()` -toimintoa, Python tarkistaa antamasi polun ja etsii sieltä tiedostoa tai hakemistoa, jonka nimeä olet antanut. Tämä toiminto palauttaa `True`-arvon, jos tiedosto tai hakemisto löytyy, ja `False`-arvon, jos sitä ei löydy.

On myös tärkeää huomata, että `os.path.exists()` toimii absoluuttisella polulla, eli koko polku täytyy antaa. Jos haluat tarkistaa hakemiston tai tiedoston olemassaolon sitä ympäröivässä kansiossa, voit käyttää `os.path.abspath()` -toimintoa, joka muuttaa suhteellisen polun absoluuttiseksi.

## Katso myös

- [Pythonin virallinen ohjeistus hakemistojen käsittelystä](https://docs.python.org/3/library/os.path.html)
- [Lyhyt opas tiedostojen ja hakemistojen käsittelyyn Pythonissa](https://realpython.com/working-with-files-in-python/)
- [Muut hyödylliset Python-toiminnot tiedostojen ja hakemistojen hallintaan](https://www.programiz.com/python-programming/file-operation)