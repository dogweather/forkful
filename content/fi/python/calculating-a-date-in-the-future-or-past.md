---
title:                "Python: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Laskeminen tulevaisuuden tai menneisyyden päivämäärään voi olla hyödyllistä esimerkiksi suunnitellessasi matkaa tai tapahtumaa. Se voi myös auttaa sinua ymmärtämään ajan kulumista ja päivämäärien välisiä suhteita.

## Kuinka tehdä

```Python
import datetime

# Lasketaan tuleva päivämäärä päivien määrällä
tuleva_paiva = datetime.date.today() + datetime.timedelta(days=7)
print(tuleva_paiva)

# Lasketaan menneinen päivämäärä päivien määrällä
menneinen_paiva = datetime.date.today() - datetime.timedelta(days=14)
print(menneinen_paiva)

# Muokataan tulos haluttuun muotoon
print("Tuleva päivämäärä:", tuleva_paiva.strftime("%d.%m.%Y"))
print("Menneinen päivämäärä:", menneinen_paiva.strftime("%d.%m.%Y"))
```

Käyttäen Pythonin sisäänrakennettua datetime-kirjastoa, voit helposti laskea tulevan tai menneen päivämäärän haluamallasi päivämäärämäärällä. Pythonin strftime-metodi auttaa muokkaamaan päivämäärän haluamaasi muotoon.

## Syvemmälle

Pythonin datetime-kirjasto tarjoaa myös muita hyödyllisiä työkaluja päivämäärien käsittelyyn. Voit esimerkiksi muuttaa päivämäärän eri aikavyöhykkeelle, laskea päivämäärien erotuksen tai tarkistaa mikä viikonpäivä päivämäärällä oli. Kannattaa tutustua datetime-kirjaston dokumentaatioon ja löytää lisää tapoja hyödyntää sitä.

## Katso myös

- Pythonin datetime-kirjaston dokumentaatio: https://docs.python.org/3/library/datetime.html
- Hyödyllisiä vinkkejä päivämäärien käsittelyyn Pythonissa: https://www.programiz.com/python-programming/datetime