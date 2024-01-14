---
title:    "Python: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi: Laskelmoidessa päivämäärää tulevaisuudessa tai menneisyydessä kannattaa käyttää Python-ohjelmointia.

Python-ohjelmointikieli tarjoaa monipuolisia työkaluja päivämäärän laskemiseen tulevaisuudessa tai menneisyydessä. Tämä mahdollistaa käyttäjän helposti laskea erilaisia päivämääriä ja suunnitella tulevaisuuden tapahtumia tai tarkastella menneitä tapahtumia.

## Miten: Esimerkkikoodi ja tuloste "```Python ...```" koodilohkoissa.

Päivämäärän laskeminen Pythonilla on helppoa. Ensimmäiseksi on määriteltävä datetime-objekti, joka sisältää päivämäärän ja ajan. Tämän jälkeen käytetään timedelta-funktiota määrittämään päivämäärän lisäys tai vähennys tietty määrä päiviä, kuukausia tai vuosia.

Esimerkiksi jos haluat laskea päivämäärän 30 päivää tulevaisuuteen nykyisestä päivämäärästä, voit käyttää seuraavaa koodia:

```Python
from datetime import date, datetime, timedelta

tanaan = date.today()
tuleva_paiva = tanaan + timedelta(days=30)

print("Päivämäärä 30 päivää tulevaisuudessa on", tuleva_paiva)
```

Tuloste näyttää päivämäärän 30 päivää nykyisen päivämäärän jälkeen muodossa "Päivämäärä 30 päivää tulevaisuudessa on [päivämäärä]".

## Syväsukellus: Tarkempaa tietoa päivämäärän laskemisesta tulevaisuudessa tai menneisyydessä.

Python tarjoaa monipuolisia toimintoja päivämäärien laskemiseen. Timedelta-funktion lisäksi voidaan käyttää myös relativedelta-funktiota, joka mahdollistaa päivämäärän laskemisen myös kuukausien ja vuosien osalta. Lisäksi Python tarjoaa myös dateutil-kirjaston, joka sisältää monia valmiita toimintoja päivämäärien laskemiseen ja muokkaamiseen.

On myös tärkeää huomata, että Python-kieli käyttää Gregoriaanista kalenteria ja se ottaa huomioon karkausvuodet automaattisesti päivämäärä-laskelmia tehdessä.

## Katso myös:

- [Pythonin viralliset dokumentaatiot datetime-moduulista](https://docs.python.org/fi/3.9/library/datetime.html)
- [Ohjeet datetime-toimintojen käyttämiseen](https://www.w3schools.com/python/python_datetime.asp)
- [Dateutil-kirjaston dokumentaatiot](https://dateutil.readthedocs.io/en/stable/)