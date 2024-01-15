---
title:                "Päivämäärän hankkiminen"
html_title:           "Python: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluaisit käyttää Pythonin nykyistä versiota saadaksesi päivämäärän. Päivämäärän hakeminen on tärkeää monissa ohjelmoinnin sovelluksissa, kuten laskurit, kalenterit tai tapahtumien aikaleimat.

## Miten

Python tarjoaa valmiin moduulin päivämäärän hallintaan, joka tekee siitä helppoa ja kätevää. Voit käyttää "datetime" -moduulia saadaksesi nykyisen päivämäärän ja ajan, kuten alla olevassa esimerkissä:

```Python
from datetime import datetime
nykyinen_päivä = datetime.now()
print(nykyinen_päivä)
```

Tämä koodi tuottaa seuraavan tulosteen:

2021-06-29 13:57:12.993947

Nyt voit käyttää saamiasi päivämäärätietoja eri tavoin ohjelmasi sisällä.

## Syventyvä sukellus

Datetime-moduuli tarjoaa monia muita toimintoja päivämäärän ja ajan hallintaan. Voit esimerkiksi muotoilla päivämäärän haluamallasi tavalla käyttämällä "strftime" -funktiota. Tämä koodi esimerkiksi tulostaa nykyisen päivämäärän muodossa "päivä, kuukausi vuosi":

```Python
from datetime import datetime
nykyinen_päivä = datetime.now()
muotoiltu_päivä = nykyinen_päivä.strftime("%d, %B %Y")
print(muotoiltu_päivä)
```

Tulostus on seuraava:

29, kesäkuu 2021

Voit myös tehdä laskelmia eri päivämäärien välillä käyttämällä "timedelta" -funktiota. Tämä koodi esimerkiksi lisää 30 päivää nykyiseen päivämäärään ja tulostaa lopputuloksen:

```Python
from datetime import datetime, timedelta
nykyinen_päivä = datetime.now()
uusi_päivä = nykyinen_päivä + timedelta(days=30)
print(uusi_päivä)
```

Tulostus on seuraava:

2021-07-29 13:57:12.993947

Voit tutkia lisää datetime-moduulin toimintoja [Pythonin virallisilla verkkosivuilla](https://docs.python.org/3/library/datetime.html).

## Katso myös

- [Pythonin viralliset verkkosivut](https://www.python.org/)
- [DateTime-moduulin dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [Pythonin datetime opetusohjelma](https://realpython.com/python-datetime/)
- [PyDateTime - datetime-moduulin dokumentaatio C-kielisille laajennoksille](https://docs.python.org/3/c-api/datetime.html)