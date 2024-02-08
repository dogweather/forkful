---
title:                "Komennoriviparametrien lukeminen"
aliases:
- fi/python/reading-command-line-arguments.md
date:                  2024-01-20T17:56:54.404931-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Komentoriviparametrien lukeminen mahdollistaa käyttäjän antamien arvojen vastaanottamisen suoraan ohjelmaasi. Tätä tehdään, koska se mahdollistaa ohjelman toiminnan mukauttamisen lennossa ilman koodin muuttamista.

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
