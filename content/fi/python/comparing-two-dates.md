---
title:                "Kahden päivämäärän vertailu"
html_title:           "Python: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertailemalla kahta päivämäärää Pythonin avulla voit tarkastella erilaisten tapahtumien tai tietojen välisiä aikajakoja ja löytää mahdollisia yhteyksiä tai syitä niiden välillä.

## Kuinka

Vertaamalla kahta päivämäärää Pythonilla voidaan käyttää datetime-kirjastoa. Ensimmäiseksi tuodaan kirjasto käyttöön komennolla ```Python
import datetime``` Tämän jälkeen voidaan luoda kaksi erillistä datetime-objektia käyttämällä ```Python
date()``` -metodia, jossa määritellään päivämäärä parametrien avulla. Vastaavasti voidaan myös luoda datetime-objekti ajan määrittelyllä tunneittain, minuuttien ja sekuntien avulla. Tämän jälkeen voidaan käyttää vertailuoperaattoreita, kuten ```Python
==, !=, >, <, >=, <=``` saadaksemme vertailutuloksen. Alla on esimerkki:

```Python
import datetime

date1 = datetime.date(2021, 1, 1)
date2 = datetime.date(2021, 1, 15)

if date2 > date1:
    print("Date2 is later than Date1")
```
Tulostus: Date2 is later than Date1

## Syventävä sukellus

Vertailemalla kahta päivämäärää voidaan käyttää myös muita hyödyllisiä datetime-kirjaston metodeja, kuten ```Python
weekday()``` joka palauttaa viikonpäivän numerona, tai ```Python
isoformat()``` joka palauttaa päivämäärän ISO-formaatissa. Lisäksi voi olla hyödyllistä tarkastella myös aikavälien tai päivämäärien välisiä eroja käyttämällä ```Python
timedelta()``` -metodia.

## Katso myös

- Pythonin virallinen datetime-dokumentaatio: https://docs.python.org/3/library/datetime.html
- Ohjeet date-objektin luomiseksi: https://www.w3schools.com/python/python_datetime.asp
- Esimerkkejä date-objektin vertailusta: https://www.programiz.com/python-programming/datetime/compare-dates