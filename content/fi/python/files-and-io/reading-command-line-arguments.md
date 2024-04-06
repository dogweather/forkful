---
date: 2024-01-20 17:56:54.404931-07:00
description: "How to: Kuinka se tehd\xE4\xE4n: Pythonissa komentoriviparametrit luetaan\
  \ `sys.argv` listan avulla. T\xE4ss\xE4 yksinkertainen esimerkki."
lastmod: '2024-04-05T21:53:57.714513-06:00'
model: gpt-4-1106-preview
summary: ''
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
Kuinka se tehdään:

Pythonissa komentoriviparametrit luetaan `sys.argv` listan avulla. Tässä yksinkertainen esimerkki:

```Python
import sys

if len(sys.argv) > 1:
    print(f"Hei, {sys.argv[1]}!")
else:
    print("Hei, tuntematon käyttäjä!")
```

Jos tallennat tämän tiedostoon `tervehdi.py` ja suoritat sen komennolla `python tervehdi.py Maailma`, saat tulosteeksi:

```
Hei, Maailma!
```

## Deep Dive
Syväsukellus:

Komentoriviparametrien lukemisen juuret ovat UNIX-järjestelmien varhaisissa päivissä. Pythonin `sys`-moduuli on ollut osa kieltä sen varhaisesta vaiheesta lähtien, antaen pääsyn komentoriviparametreihin. Vaihtoehtoisia tapoja komentoriviparametrien käsittelyyn tarjoavat kirjastot, kuten `argparse` ja `click`, jotka tuovat lisätoimintoja kuten automaattiset ohjeviestit ja tyyppitarkistukset.

Python-scriptejä suoritettaessa `sys.argv` listan ensimmäinen alkio on aina skriptin nimi tai tyhjä string, jos Pythonia käytetään interaktiivisesti. Muut alkiot ovat käyttäjän komentorivillä antamia argumentteja.

## See Also
Lisätietoja:

Lue lisää komentoriviparametreista ja niiden käsittelystä Pythonin virallisessa dokumentaatiossa:
- [sys.argv - Python documentation](https://docs.python.org/3/library/sys.html#sys.argv)
- [argparse - Command-line option and argument parsing](https://docs.python.org/3/library/argparse.html)
- [click - A Python package for creating beautiful command line interfaces](https://click.palletsprojects.com/en/7.x/)
