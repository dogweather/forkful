---
title:                "Python: Kahden päivämäärän vertailu"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertaaminen on tärkeä taito useissa Pythonin ohjelmoinnin sovelluksissa. Se voi auttaa sinua tarkistamaan, ovatko kaksi päivämäärää samat tai järjestyksessä. Se voi myös auttaa sinua laskemaan aikaa kahden päivämäärän välillä. Päivämäärien vertaaminen on hyödyllistä esimerkiksi projektinhallinnassa, kun haluat tarkistaa, onko tehtävä tehty ennen tiettyä päivämäärää.

## Kuinka verrata kahta päivämäärää?

Päivämäärien vertaaminen Pythonissa on yksinkertaista ja helppoa. Voit käyttää sisäänrakennettua `datetime`-kirjastoa ja sen `date`-objektia vertailemaan päivämääriä.

```Python
from datetime import date

# Luodaan kaksi päivämäärää
pvm1 = date(2020, 1, 1)
pvm2 = date(2020, 1, 15)

# Vertaillaan päivämääriä
if pvm1 < pvm2:
    print("Päivämäärä 1 on ennen päivämäärää 2")
elif pvm1 == pvm2:
    print("Päivämäärät ovat samat")
else:
    print("Päivämäärä 2 on ennen päivämäärää 1")
```

Output:
```
Päivämäärä 1 on ennen päivämäärää 2
```

Voit myös käyttää `timedelta`-objektia laskemaan aikaa kahden päivämäärän välillä.

```Python
from datetime import date, timedelta

# Luodaan kaksi päivämäärää
pvm1 = date(2020, 1, 1)
pvm2 = date(2020, 1, 15)

# Lasketaan päivien välinen ero
ero = pvm2 - pvm1

print("Päivien ero:", ero.days)
```

Output:
```
Päivien ero: 14
```

## Syvempi sukellus päivämäärien vertailuun

Kun vertaat kahta päivämäärää Pythonissa, on tärkeää huomata, että `date`-objektin tulee olla sama muoto molemmissa päivämäärissä. Tämä tarkoittaa, että päivämäärät tulee olla syötetty samassa järjestyksessä, vuosi-kuukausi-päivä.

Voit myös käyttää `datetime`-objektia, joka sisältää päivän ja ajan tiedot, vertailemaan päivämääriä. Voit lukea lisää `datetime`-objektista Pythonin dokumentaatiosta.

## Katso myös

- [Pythonin datetime-kirjaston dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [Pythonin timedelta-kirjaston dokumentaatio](https://docs.python.org/3/library/datetime.html#timedelta-objects)