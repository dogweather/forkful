---
title:    "Python: Nykyisen päivämäärän hankkiminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan miettinyt, kuinka voit saada selville nykyisen päivämäärän ja ajan Python-ohjelmoinnin avulla? Tämä taito voi olla hyödyllinen esimerkiksi tietokannan hallinnassa tai aikaleimojen luomisessa tiedostoille. Jatka lukemista ja opi kuinka!

## Kuinka

```Python
import datetime  # Tuodaan datetime moduuli

nykyinen_paiva = datetime.datetime.now()  # Tallennetaan nykyinen päivä datetime objektiin

print(nykyinen_paiva)  # Tulostetaan nykyinen päivä ja aika

```

```Python
import datetime  # Tuodaan datetime moduuli

nykyinen_paivamaara = datetime.date.today()  # Tallennetaan nykyinen päivämäärä date objektiin
nykyinen_aika = datetime.datetime.now().time()  # Tallennetaan nykyinen aika time objektiin

print(nykyinen_paivamaara)  # Tulostetaan nykyinen päivämäärä
print(nykyinen_aika)  # Tulostetaan nykyinen aika

```

**Tulostus:**

```
2020-09-24 20:08:30  # Ensimmäisen koodinpätkän tulostus
2020-09-24  # Toisen koodinpätkän tulostus
20:08:30.903726  # Toisen koodinpätkän tulostus
```

## Syvä sukellus

Pythonin datetime-moduuli tarjoaa useita toimintoja nykyisen päivämäärän ja ajan hankkimiseen. `now()`-funktio palauttaa nykyisen päivän ja ajan datetime-objektina. Voit myös käyttää `today()`-funktiota saadaksesi vain nykyisen päivämäärän tai `time()`-funktiota saadaksesi vain nykyisen ajan.

Voit myös käyttää `strftime()`-funktiota muokataksesi päivämäärän tai ajan esitysmuotoa. Esimerkiksi, `strftime("%d.%m.%y")` muuttaa päivämäärän muotoon DD.MM.YY.

## Katso myös

- [Pythonin datetime-moduulin dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [W3Schoolsin esimerkkejä nykyisen päivämäärän ja ajan hakemisesta Pythonilla](https://www.w3schools.com/python/python_datetime.asp)
- [Real Pythonin opetusohjelma nykyisen päivämäärän ja ajan käsittelystä Pythonilla](https://realpython.com/python-datetime/)