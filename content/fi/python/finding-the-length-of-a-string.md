---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden määrittäminen on toimenpide, jossa lasketaan merkkijonon merkkien määrä. Ohjelmoijat tekevät tämän mm. tiedonkäsittelyn, inputin validoinnin ja tiedon karsinnan tarpeisiin.

## Miten: 
Pythonissa merkkijonon pituuden saa selville `len()`-funktiolla. Esimerkiksi:

```Python
sana = "Ohjelmointi"
print(len(sana))     # Tulostaa: 12
```
Tai voit käsitellä käyttäjän syötettä seuraavasti: 

```Python
sana = input("Anna sana: ")
print("Antamasi sanan pituus on", len(sana), "merkkiä.")
```
## Syväsukellus
Merkkijonon pituuden määrittämisen tarve on ollut olemassa jo ohjelmoinnin alkutaipaleesta lähtien, jolloin muistin ja tallennustilan optimointi olivat kriittisiä haasteita.

Pythonissa `len()`-funktio on yleisimmin käytetty, mutta on myös mahdollista laskea merkkijonon pituus iteroimalla sen yli, esimerkiksi `for`-silmukan avulla:

```Python
sana = "Ohjelmointi"
pituus = 0
for merkki in sana:
    pituus += 1
print(pituus)     # Tulostaa: 12
```

Pythonin `len()`-funktion toteutus perustuu CPythonin sisäiseen `PyObject_Length` -funktioon, joka pyrkii nopeuteen ja tehokkuuteen.

## Katso myös
[Pythonin len-funktio: Pythonin virallinen dokumentaatio](https://docs.python.org/3/library/functions.html#len)
[Merkkijonojen käsittely Pythonilla: Tietojenkäsittelytieteen perusteet](https://2018-ohjelmointi.github.io/part2/05_merkkijonot/)