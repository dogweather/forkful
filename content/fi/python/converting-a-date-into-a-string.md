---
title:    "Python: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen ohjelmoija tietää, kuinka tärkeää on käsitellä päivämääriä oikein. Niitä käytetään erilaisissa sovelluksissa eri tarkoituksiin, kuten tapahtumien aikatauluttamiseen, tiedostojen luomiseen tai jopa laskutukseen. Usein päivämäärän esittäminen tekstillisessä muodossa on välttämätöntä ja tässä blogikirjoituksessa kerromme, kuinka voit muuntaa päivämäärän tekstimuotoon helposti Pythonissa.

## Kuinka
Eri ohjelmointikielissä on erilaisia tapoja käsitellä päivämääriä. Pythonin datetime-moduulilla on kuitenkin helppo muuttaa päivämäärä tekstimuotoon ```strftime()```-funktiolla. Alla on esimerkki koodista, joka muuntaa päivämäärän tekstimuotoon ja tulostaa sen:

```python
import datetime

päivämäärä = datetime.date(2021, 6, 1)

päivämäärä_str = päivämäärä.strftime('%d.%m.%Y')

print(päivämäärä_str)
```
Tämä koodi tuottaa seuraavan tulosteen: ```01.06.2021```

Kuten huomaat, ```strftime()```-funktio käyttää merkkijonoa muodostaakseen päivämäärästä halutun muodon. Tarkemman luettelon käytettävissä olevista merkkijonoista voit löytää Pythonin virallisesta dokumentaatiosta.

## Syvähdyksellä
Päivämäärän muuttaminen tekstimuotoon voi vaikuttaa yksinkertaiselta, mutta syvemmin diveeramalla voit huomata muutamia tärkeitä huomioitavia asioita.

Ensinnäkin, huomaa, että ```strftime()```-funktio käyttää nollapäällä aloitusta, mikä tarkoittaa, että jos päivät, kuukaudet tai tunnit ovat yksinumeroinen luku, niiden eteen tulee lisätä nolla. Tämä varmistaa, että päivämäärä esitetään aina samassa muodossa.

Toiseksi, on tärkeää huomioida, että mikä tahansa muotoon vaikuttava muutos tehdään alkuperäiselle päivämäärälle itselleen. Tämä tarkoittaa, että jos haluat muuttaa päivämäärän tekstiksi, mutta myös käyttää alkuperäistä päivämäärääsi jatkossa, sinun on luotava uusi muuttuja.

Nämä ovat vain muutamia asioita, jotka on hyvä pitää mielessä päivämäärän muuntaessaan tekstimuotoon. Kannattaa tutustua tarkemmin Pythonin datetime-moduuliin löytääksesi muita hyödyllisiä toimintoja ja temppuja päivämäärän käsittelemiseen.

## Katso myös
- [Pythonin virallinen dokumentaatio datetime-moduulista](https://docs.python.org/3/library/datetime.html)
- [DateFormat-paketin käyttö viimeisimmän päivämäärän generoinnissa](https://www.educba.com/dateformat-in-python/) 
- [Päivämäärän esittäminen eri kielillä Pythonilla](https://stackabuse.com/how-to-format-dates-in-python/)