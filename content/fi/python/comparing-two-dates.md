---
title:    "Python: Kahden päivämäärän vertailu"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmoijat joutuvat käsittelemään päivämääriä sovelluksissaan. Voi olla tarpeellista verrata kahta päivämäärää toisiinsa esimerkiksi laskemaan aikaa kahden tapahtuman välillä tai tarkistamaan, onko tietty päivämäärä menneisyydessä vai tulevaisuudessa. Tässä blogikirjoituksessa opit vertailemaan kahta päivämäärää Pythonin avulla.

## Kuinka tehdä
Vertaaminen kahden päivämäärän välillä on helppoa Pythonissa. Ensimmäinen askel on tuoda datetime-moduuli käyttöön.

```Python
import datetime
```

Sitten voit luoda kaksi datetime-objektia, jotka sisältävät vertailtavat päivämäärät.

```Python
first_date = datetime.datetime(2020, 5, 12)
second_date = datetime.datetime(2021, 2, 10)
```

Voit nyt käyttää vertailuoperaattoreita, kuten <, > tai ==, vertaamaan päivämääriä. Alla olevassa esimerkissä tarkistetaan, onko first_date myöhäisempi kuin second_date.

```Python
if first_date > second_date:
    print("first_date on myöhäisempi")
```
Output:
```
first_date on myöhäisempi
```

Voit myös vähentää kahden päivämäärän välistä aikaa timedelta-objektilla.

```Python
date1 = datetime.datetime(2021, 4, 5)
date2 = datetime.datetime(2021, 3, 4)
difference = date1 - date2
print(difference.days)
```
Output:
```
32
```

## Syventävää tietoa
Vertailu kahden päivämäärän välillä perustuu niiden arvojen vertailuun. Jos molemmat päivämäärät ovat datetime-objekteja, niiden vertailu perustuu niiden timestamp-arvojen vertailuun. Timestamp-arvo kertoo kuinka paljon sekunteja on kulunut vuoden 1970 tammikuun ensimmäisestä päivästä kyseiseen päivämäärään.

On myös tärkeää huomata, että datetime-objektit ovat muuttumattomia. Tämä tarkoittaa sitä, että kun luot datetime-objektin, sitä ei voi enää muuttaa. Voit kuitenkin luoda uuden datetime-objektin käyttämällä esimerkiksi timedelta-objektia.

## Katso myös
- [Pythonin datetime-moduuli](https://docs.python.org/3/library/datetime.html)
- [Vertailuoperaattorit Pythonissa](https://docs.python.org/3/reference/expressions.html#value-comparisons)
- [Timedelta-objekti Pythonissa](https://docs.python.org/3/library/datetime.html#datetime.timedelta)