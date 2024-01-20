---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen generointi tietojenkäsittelyssä merkitsee lukuja, jotka ovat lain mukaan satunnaisesti valitut. Ohjelmoijat tekevät näin simulaatioiden, pelien, turvallisuuden ja tietojenkäsittelyalgoritmiensa vuoksi.

## Miten:

Pythonissa satunnaislukujen luominen on erittäin yksinkertaista. 'random' kirjasto tekee sen.

```Python
import random

# Generoi satunnainen luku väliltä 0 ja 1
x = random.random()
print(x)

# Generoi satunnainen kokonaisluku väliltä 1 ja 10
y = random.randint(1, 10)
print(y)
```

Esimerkiksi, käytön jälkeen, saatat saada tuloksen, kuten:

```Python
0.35461964653229205
7
```
Tässä 0.35461964653229205 ja 7 ovat Pythonin generoimat satunnaisluvut.

## Syvempi tieto:

Satunnaislukujen generointi ei ole uusi käsite, ja sitä on käytetty vuosisatojen ajan pelien kuten nopan ja korttipelien yhteydessä. Pythonin 'random' kirjasto perustuu todellakin perinteiseen 'C' kirjastoon ja se luo pseudo-satunnaislukuja, joka tarkoittaa, että ne näyttävät satunnaisilta, mutta niitä voidaan toistaa, jos tunnet algoritmin ja siementen.

Vaihtoehtona Pythonin random kirjastolle voit myös käyttää numpy-kirjastoa monimutkaisempiin toimintoihin. Lisäksi uuid-kirjasto antaa sinun generoida satunnaisia UUID-koodilohkoja.

## Katso myös:

1. Pythonin dokumentaatio: https://docs.python.org/3/library/random.html
2. Numpy-kirjasto satunnaislukujen generointiin: https://numpy.org/doc/stable/reference/random/index.html
3. UUID-kirjasto: https://docs.python.org/3/library/uuid.html
4. Wikipedia-sivu pseudo-satunnaislukugeneraattorista: https://fi.wikipedia.org/wiki/Pseudosatunnaislukugeneraattori