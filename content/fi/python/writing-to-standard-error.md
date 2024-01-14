---
title:                "Python: Tietokoneohjelmoinnin kirjoittaminen standardivirheeseen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Writing to standard error on tärkeä taito jokaiselle Python-ohjelmoijalle. Se auttaa tunnistamaan mahdollisia virheitä ohjelmassa ja korjaamaan ne nopeasti.

## Kuinka tehdä

Seuraavassa koodiesimerkissä näytämme, kuinka kirjoitetaan standardi error käyttäen `sys`-moduulia:

```Python
import sys

sys.stderr.write("Virhe: Tämä on esimerkki virheilmoituksesta.")
```
Tämä tulostaa seuraavan viestin terminaaliin tai komentokehotteeseen:

```
Virhe: Tämä on esimerkki virheilmoituksesta.
```

## Syvälle

Standardi error (eli `stderr`) on yksi kolmesta tiedostovirtoihin liittyvästä virtauksesta Pythonissa. Toinen on standardi syöte (eli `stdin`) ja kolmas on standardi tulo (eli `stdout`).

Virheilmoitusten ohjaaminen standardi erroriin on hyödyllistä silloin, kun haluamme erottaa virheviestit tavallisista tulosteista. Näin voimme helposti tunnistaa ja käsitellä virheitä ohjelman suorituksen aikana.

On myös mahdollista ohjata virheilmoitukset log-tiedostoihin tai muille tiedostovirroille, jotka voivat auttaa pitämään ohjelman suorituksen siistinä ja jäljitettävänä.

## Katso myös

- [Pythonin `sys`-moduuli](https://docs.python.org/3/library/sys.html)
- [Virheviestien hallinta Pythonissa](https://realpython.com/python-exceptions/)
- [Tulostaminen ja virheiden käsittely Pythonissa](https://realpython.com/python-print/)