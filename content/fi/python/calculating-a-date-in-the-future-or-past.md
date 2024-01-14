---
title:                "Python: Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi laskea tulevaisuuden tai menneen päivämäärän? Se voi olla hyödyllistä esimerkiksi suunnitellessa lomamatkaa tai muuta tärkeää tapahtumaa. Myös historiallisten tapahtumien muistaminen voi olla syy laskea menneitä päiviä.

## Miten tehdä
Laskeminen tulevaisuuden tai menneen päivämäärän voi olla yksinkertaista Pythonin avulla. Seuraavassa esimerkissä laskemme päivämäärän kahden viikon päähän.

```Python
#importoidaan datetime moduuli
import datetime

#luodaan date-objekti nykyisestä päivämäärästä
current_date = datetime.date.today()

#lisätään kaksi viikkoa nykyiseen päivämäärään
future_date = current_date + datetime.timedelta(weeks=2)

#tulostetaan tuleva päivämäärä muodossa "pp.kk.vvvv"
print(future_date.strftime("%d.%m.%Y"))
```

Tulostus:
```
23.09.2020
```

## Syvemmälle
Pythonin datetime-moduuli tarjoaa erilaisia toimintoja päivämäärän laskemiseen ja muokkaamiseen. Voit esimerkiksi lisätä päivän, kuukauden tai vuoden nykyiseen päivämäärään tai laskea eron kahden päivämäärän välillä. Voit myös muuntaa päivämäärän eri muotoihin tai hakea tiettyä tietoa, kuten viikonpäivä tai viikonnumero.

## Katso myös
- [Pythonin datetime-moduulin dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [Artikkeli: "Calculating future dates in Python using datetime"](https://tecadmin.net/calculate-future-dates-python-datetime/)