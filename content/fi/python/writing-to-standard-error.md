---
title:                "Python: Puutetoiminnan kirjoittaminen"
simple_title:         "Puutetoiminnan kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa standardivirheeseen?

Standardivirhe on tärkeä työkalu ohjelmoijille, sillä se antaa mahdollisuuden nähdä ja tallentaa virheviestejä koodin suorituksen aikana. Näin pystytään helposti löytämään ja korjaamaan virhetilanteita ja varmistamaan ohjelman toimivuus.

## Miten kirjoittaa standardivirheeseen?

Standardivirheeseen kirjoittaminen on yksinkertaista käyttämällä Pythonin "sys" -kirjastoa ja sen "stderr" -olioa. Seuraavassa esimerkissä näytetään, miten voit tulostaa virheviestin "Hei, tämä on virhe!" standardivirheeseen:

```Python
import sys

sys.stderr.write("Hei, tämä on virhe!")
```

Tämän jälkeen suorittaessa koodia, virheviesti tulostuu suoraan terminaalin konsoliin.

```
Hei, tämä on virhe!
```

Voit myös ottaa käyttöön "try-catch" -lausekkeen virheiden käsittelyä varten ja käyttää "sys.exc_info()" -funktiota tulostamaan tarkemman virheilmoituksen.

```Python
try:
  # Koodi, joka saattaa aiheuttaa virheen
except:
  # Tarkista ja tulosta virheviesti
  error = sys.exc_info()[0]
  sys.stderr.write("Virhe: " + str(error))
```

## Syvempi sukellus standardivirheeseen

Standardivirheen käyttöä voi myös räätälöidä tarpeidesi mukaan. Voit esimerkiksi ohjata virheviestit tietokantatauluihin tai tallentaa ne erilliseen tiedostoon käyttämällä "sys.stderr.redirect()" -funktiota. Voit myös laajentaa standardivirheen käyttöä käsittelemällä erilaisia virhetilanteita ja tulostamalla niihin liittyvää tietoa.

Standardivirheen käyttö voi auttaa sinua tehokkaammin käsittelemään ja havaitsemaan virheitä koodissasi. Se on hyödyllinen työkalu kaikille ohjelmoijille ja auttaa parantamaan ohjelmien luotettavuutta.

# Katso myös

- [Python "sys" -kirjasto](https://docs.python.org/3/library/sys.html)
- [Virheiden käsittely Pythonissa](https://realpython.com/python-exceptions/)
- [Standardivirheen käyttö esimerkkien kera](https://www.programiz.com/python-programming/exception-handling)