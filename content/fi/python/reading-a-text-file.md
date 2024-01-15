---
title:                "Tekstitiedoston lukeminen"
html_title:           "Python: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston lukeminen on yksi yleisimmistä ohjelmoinnin tehtävistä. Se avaa ovia tiedonkäsittelyyn ja tiedon analysointiin. Lukemalla tekstiä, voit hakea tiettyjä tietoja tai suorittaa muokkauksia tiedostoon liittyvien operaatioiden suorittamiseksi.

## Ohjeet

```python
# Avaa teksti-tiedosto lukemista varten
file = open('tekstitiedosto.txt', 'r')

# Lue tiedosto rivi kerrallaan
for line in file:
  print(line)

# Sulje tiedosto
file.close()
```

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
Tämä on kolmas rivi.
```

Tässä esimerkissä käytämme `open()`-funktiota avaamaan halutun tekstitiedoston lukemista varten ja annamme `r` -parametrin, joka tarkoittaa "lue"-tilaa. Voit myös antaa `w` parametrin, joka tarkoittaa "kirjoitus"-tilaa, jos haluat muokata tai luoda uuden tekstitiedoston. Seuraavaksi käytämme `for`-silmukkaa lukeaksemme tiedoston rivi kerrallaan ja tulostamme sen näytölle. Lopuksi on tärkeää muistaa sulkea tiedosto `close()`-funktiolla, jotta vapautamme resurssit.

## Syvällisempi sukellus

Tekstitiedoston lukemiseen on olemassa myös muita tapoja. Voit esimerkiksi määrittää tiedostopolun ja luoda `with`-lausekkeen käyttääksesi tiedostoa. Tämä tapa lukita ja sulkea tiedoston automaattisesti, kun `with`-lohko suoritetaan. Voit myös käyttää `read()`- ja `readlines()`-funktioita, jotka lukevat koko tiedoston tai kaikki rivit listana ja voit käsitellä tiedoston sisältöä haluamallasi tavalla.

## Katso myös

- [Pythonin viralliset dokumentit - Tiedostojen käsittely](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python - Tiedostojen käsittely Pythonissa](https://realpython.com/read-write-files-python/)
- [Tekstitiedoston merkistöongelmien ratkaiseminen Pythonissa](https://www.journaldev.com/23749/python-read-text-file)