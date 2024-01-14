---
title:    "Python: Lukemalla komentoriviparametrit"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi 

Luet tämän blogitekstin, koska haluat oppia lukemaan komentoriviargumentteja Pythonilla. Komentoriviargumentit ovat tärkeitä, koska ne mahdollistavat ohjelmiesi dynaamisen ja interaktiivisen toiminnan.

## Miten tehdä

Komentoriviargumenttien lukeminen on yksinkertaista Pythonilla. Voit käyttää `sys` -kirjastoa ja sen `argv` -metodia. Alla on esimerkki koodi:

```Python
import sys

args = sys.argv
print("Komentoriviargumentit: ", args)
```

Tämä koodi tulostaa kaikki annetut komentoriviargumentit. Voit myös käyttää indeksointia saadaksesi tietyn argumentin. Esimerkiksi `args[1]` tulostaisi ensimmäisen argumentin.

Voit myös antaa ohjelmalle komentoriviargumentteja suoraan suorittaessasi sitä. Esimerkiksi `python ohjelma.py argumentti1 argumentti2` antaisi ohjelmalle kaksi argumenttia `argumentti1` ja `argumentti2`.

## Syvempi sukellus 

Kommentoriviargumenttien lukemisen taustalla on käyttöjärjestelmäsi ydin. Käyttöjärjestelmäsi lähettää argumentit ohjelmalle suoritettaessa ja `sys.argv` kokoaa ne listaksi, josta ne voidaan helposti käsitellä Pythonilla.

Komentoriviargumentteja käytetään usein ohjelmien muokkaamiseen suorituksen aikana. Niitä voidaan myös käyttää ohjelmien konfigurointiin ja tiedostojen käsittelyyn.

## Katso myös 

- [Pythonin virallinen dokumentaatio komentoriviargumenttien käsittelystä](https://docs.python.org/3/library/sys.html#sys.argv)
- [Komentoriviargumenttien käyttö OReillyn "Python Cookbook" -kirjassa](https://www.oreilly.com/library/view/python-cookbook/0596001673/ch13s08.html)
- [Komentoriviargumenttien opas Real Python -sivustolla](https://realpython.com/command-line-interfaces-python-argparse/)