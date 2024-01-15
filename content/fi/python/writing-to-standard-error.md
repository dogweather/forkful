---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Python: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kun käytät Pythonia kehitystyössä, saatat törmätä virheisiin, jotka eivät näy tavallisessa tulostuksessa. Näiden virheiden syiden selvittämiseksi ja korjaamiseksi sinun on kirjoitettava standardiin virheeksi.

## Miten

Kun haluat kirjoittaa tietyn rivin standardin virheeksi, käytä "sys" moduulin "stderr" ominaisuutta koodissa, kuten seuraavassa esimerkissä:

```Python
import sys

print("Tämä on normaali tulostus")

sys.stderr.write("Tämä on standardi virhe")
```

Tämä tuottaa seuraavanlaisen tulostuksen:

```
Tämä on normaali tulostus
Tämä on standardi virhe
```

## Syvenny

Viestin kirjoittaminen standardi virheeksi on hyödyllistä erityisesti silloin, kun haluat tietää, mistä virheilmoitus tulee tietyn rivin koodissa. Voit myös yhdistää "stderr.write ()" -toiminnon "try" ja "except" lohkoon, jotta voit käsitellä virheitä paremmin.

## Katso myös

- [Pythonin "sys" moduulin virallinen dokumentointi](https://docs.python.org/3/library/sys.html)
- [Täydellinen opas virheenkäsittelyyn Pythonissa](https://realpython.com/python-exceptions/)