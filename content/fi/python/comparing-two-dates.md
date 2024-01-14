---
title:                "Python: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Tulee tilanteita, jolloin meidän täytyy vertailla kahta eri päivämäärää. Tämä voi olla hyödyllistä esimerkiksi, kun haluamme tarkistaa onko tietyn tapahtuman päivämäärä aiemmin vai myöhemmin kuin toinen tapahtuma. Pythonilla tämä on helppoa toteuttaa ja se voi säästää aikaa ja vaivaa manuaalisen vertailun sijaan.

## Miten

Pythonilla on valmiina muutamia käteviä funktioita, jotka helpottavat päivämäärien vertailua. Ensimmäinen näistä on `datetime` moduuli, joka sisältää `date` luokan. Tämän luokan avulla voimme luoda päivämääriä ja suorittaa vertailuja niiden välillä.

```Python
# Tuodaan datetime moduuli
import datetime

# Luodaan kaksi eri päivämäärää: ensimmäinen maaliskuun 1. ja toinen huhtikuun 1.
date1 = datetime.date(2021, 3, 1)
date2 = datetime.date(2021, 4, 1)

# Vertaillaan päivämääriä ja tulostetaan tulos
if date1 < date2:
    print("Ensimmäinen päivämäärä on aiempi kuin toinen.")
elif date1 > date2:
    print("Ensimmäinen päivämäärä on myöhäisempi kuin toinen.")
else:
    print("Päivämäärät ovat samat.")
# Output: Ensimmäinen päivämäärä on aiempi kuin toinen.
```

Toinen tapa vertailla päivämääriä on käyttää `timedelta`-luokkaa, jolla voidaan laskea aikaa kahden päivämäärän välillä. Tämä voi olla hyödyllistä esimerkiksi jos haluamme tarkistaa kuinka monta päivää on jäljellä tiettyyn tapahtumaan.

```Python
# Luodaan kaksi eri päivämäärää: tänään ja tuleva syntymäpäivä
today = datetime.date.today()
birthday = datetime.date(2021, 10, 14)

# Lasketaan aika niiden välillä ja tulostetaan tulos
time_remaining = birthday - today
print("Syntymäpäivään on jäljellä", time_remaining.days, "päivää.")
# Output: Syntymäpäivään on jäljellä 180 päivää.
```

## Syvemmälle

Vertaillessamme kahta päivämäärää on tärkeää tietää, että päivämäärät voidaan vertailla vain samassa muodossa. Esimerkiksi, jos haluamme vertailla päivämäärää ja aikaa, meidän täytyy muuttaa ne ensin samassa muodossa oleviksi.

Lisäksi `datetime` moduuli sisältää erilaisia funktioita, joilla voi muokata ja työskennellä päivämäärien kanssa. Näitä ovat esimerkiksi `strftime()`-funktio, jolla voidaan muuttaa päivämäärän esitystapaa sekä `timedelta()`-funktio, jolla voidaan lisätä tai vähentää aikaa päivämääriin.

## Katso myös

- [Pythonin virallinen dokumentaatio](https://docs.python.org/fi/3/library/datetime.html)
- [Tutoriaali päivämäärien käsittelystä Pythonilla](https://realpython.com/python-datetime/)
- [Päivämäärien vertailun esimerkkikoodeja](https://www.geeksforgeeks.org/comparing-dates-python/)