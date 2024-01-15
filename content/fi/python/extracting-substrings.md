---
title:                "Alastringien erottaminen"
html_title:           "Python: Alastringien erottaminen"
simple_title:         "Alastringien erottaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Voi olla monia syitä, miksi haluat irrottaa osajoukkoja merkkijonoista. Ehkä haluat tarkistaa, onko tietyllä tekstinpätkällä haluamiasi ominaisuuksia, tai ehkä haluat muokata merkkijonoa jollakin tavalla.

## Miten

Irrota osajoukkoja merkkijonoista Pythonilla on helppoa käyttäen slice-merkintää. Slice-merkinnällä voit valita haluamasi osan merkkijonosta käyttämällä suluilla [ja ] ja määrittämällä alku- ja loppuindeksit. Esimerkiksi:

```Python
teksti = "Tämä on esimerkki"
print(teksti[5:8])
```

Tämä tulostaisi "on", koska se valitsi merkkijonon 5. indeksistä 8. indeksiin asti (huomaa, että viimeistä indeksiä ei sisällytetä).

Voit myös määrittää askeleen käyttämällä kolmatta arvoa slice-merkinnässä. Esimerkiksi:

```Python
teksti = "Tämä on esimerkki"
print(teksti[::2])
```

Tulostaisi "Tm neeiek", koska se valitsi joka toisen merkin merkkijonosta.

## Syvemmälle

Slice-merkintä on kätevä tapa irrottaa osajoukkoja merkkijonoista, mutta sinun kannattaa myös muistaa, että merkkijonoilla on monta muuta hyödyllistä toimintoa. Esimerkiksi voit käyttää metodia .find() etsimään tietyllä tekstinpätkällä sijaitsevan indeksin tai .replace() korvaamaan tietyt merkit toisilla. Slice-merkintä on kuitenkin nopein tapa irrottaa osajoukkoja, joten sitä kannattaa hyödyntää, kun se on mahdollista.

## Katso myös

- [Pythonin virallinen dokumentaatio osajoukkojen irrottamisesta merkkijonoista](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Stack Overflow -kysymys: "Miten irrottaa osajoukko merkkijonosta Pythonilla?"](https://stackoverflow.com/questions/663171/how-do-i-get-a-substring-of-a-string-in-python)