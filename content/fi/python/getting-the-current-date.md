---
title:                "Python: Saada nykyinen päivämäärä"
simple_title:         "Saada nykyinen päivämäärä"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisit saada nykyisen päivämäärän?

Monet ohjelmoijat haluavat saada nykyisen päivämäärän erilaisia projekteja varten. Tämä voi tuntua hyödyttömyydeltä, mutta se on itse asiassa tärkeä taito, joka voi auttaa ohjelmoijia aikaleimojen seuraamisessa ja tiedostojen järjestämisessä.

## Kuinka: Esimerkkejä koodista ja tuotosten kera "```Python ... ```" koodilohkoissa.

```Python
import datetime

tänään = datetime.date.today()
print("Tänään on:", tänään)
```

Tämä koodi käyttää Pythonin sisäänrakennettua datetime-kirjastoa saadakseen nykyisen päivämäärän ja tulostaa sen näytölle. Voit myös muokata koodia saadaksesi tietyn päivämäärän säännöllistäessäsi aikataulua projektillesi.

```Python
from datetime import date

syntymäpäivä = date(1995, 4, 10)
ikä = date.today() - syntymäpäivä
print("Ikä päivinä:", ikä.days)
```

Tässä esimerkissä käytetään datetime-kirjastoa laskemaan päivien määrä nykyisen päivämäärän ja valitun syntymäpäivän välillä. Tämä voisi olla hyödyllistä laskiessaan henkilön ikää tai seuratessaan tärkeitä päivämääriä.

## Syvällisempi tarkastelu: Tietoa nykyisen päivämäärän saamisesta.

Nykyisen päivämäärän saaminen voi tuntua yksinkertaiselta tehtävältä, mutta se voi olla monimutkaisempi kuin ajattelisi. Päivämäärät esitetään usein tiettyjen sääntöjen mukaisesti, ja niiden käsittely vaatii tietynlaista ohjelmointitaitoa. On myös tärkeää huomata, että aikavyöhykkeillä voi olla vaikutus päivämäärän saamiseen ja käsittelyyn.

Pienellä tutkimuksella ja harjoituksella voit oppia paljon nykyisen päivämäärän saamisesta ja sen käyttämisestä erilaisissa projekteissa.

## Katso myös

- [Pythonin datetime-kirjaston dokumentaatio](https://docs.python.org/fi/3/library/datetime.html)
- [Ohjeita päivämäärien käsittelyyn Pythonissa](https://www.w3schools.com/python/python_datetime.asp)
- [Lyhyt opetusvideo Pythonin datetime-kirjaston käytöstä](https://www.youtube.com/watch?v=eirjjyP2qcQ)