---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Vertaamme kahden päivämäärän eroa, ymmärtääksemme ajanjakson näiden kahden päivämäärän välillä. Ohjelmoijat tekevät tämän automatisoidakseen toimintoja ja päivämäärien tunnistamiseen liittyviä päätöksiä.

## Kuinka tehdä?
Pythonin `datetime`-kirjastossa on toiminnot, joilla voit helposti verrata kahta päivämääriä. Tässä on esimerkki:

```Python
from datetime import datetime

# Määritellään kaksi päivämäärää
date1 = datetime(2021, 12, 1)
date2 = datetime(2022, 1, 1)

# Vertaamme päivämääriä
if date1 < date2:
    print("Date1 on ennen date2")
else:
    print("Date2 on ennen date1")
```
Ohjelman tuloste on "Date1 on ennen date2" koska 1. Joulukuuta 2021 on ennen 1. Tammikuuta 2022.

## Syvempi sukellus
Historiallisesti päivämäärien vertaaminen oli monimutkaista, koska kuukaudet voivat olla eri pituisia ja vuodet voivat olla karkausvuosia. Pythonin `datetime`-kirjasto tekee tästä yksinkertaisempaa.

Vaihtoehtoisia tapoja päivämäärien vertaamiseksi ovat esimerkiksi `date2 - date1`, joka palauttaa `timedelta`-olion päivien määränä.

Jos olet kiinnostunut tietämään päivämäärien vertailun yksityiskohdista, Pythonin `datetime`-kirjasto tekee vertailun vuosien, kuukausien ja päivien perusteella kompensoiden kuukausien eripituuksia ja karkausvuosia.

## Katso myös
Lisätietoja päivämäärien vertaamisesta Pythonissa löytyy Pythonin virallisesta dokumentaatiosta:
1. Pythonin datetime-kirjaston dokumentaatio: https://docs.python.org/3/library/datetime.html
2. Pythonin timedelta-olion dokumentaatio: https://docs.python.org/3/library/datetime.html#timedelta-objects