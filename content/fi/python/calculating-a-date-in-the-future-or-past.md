---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "Python: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen laskea päivämäärä tulevaisuudessa tai menneisyydessä, esimerkiksi kun suunnittelet matkaa tai tapahtumaa ja haluat varmistaa päivämäärän tarkkuuden.

## Miten

Laskeminen tulevaisuuden tai menneisyyden päivämäärille on helppoa Pythonilla. Voit käyttää datetime-kirjastoa ja sen tarjoamia metodeja, kuten timedelta ja date.

```Python
from datetime import date, timedelta # importataan tarvittavat moduulit

tanaan = date.today() # tänään
huomenna = tanaan + timedelta(days=1) # lasketaan huominen päivämäärä
viikon_kuluttua = tanaan + timedelta(weeks=1) # lasketaan päivämäärä viikon kuluttua
tanaan_menestt = tanaan - timedelta(days=365) # lasketaan päivämäärä vuosi sitten

print(huomenna) # tulostetaan tuleva päivämäärä
print(viikon_kuluttua)
print(tanaan_menestt)
```

Output:
2021-10-29
2021-11-06
2020-10-30

Voit myös lisätä tai vähentää päivämääriltä tietyt määrät päiviä, kuukausia tai vuosia käyttämällä timedelta-metodia:

```Python
loma = tanaan + timedelta(days=30) # lasketaan päivämäärä 30 päivää tulevaisuuteen

print(loma.strftime("%d.%m.%Y")) # tulostetaan tuleva päivämäärä halutussa muodossa
```

Output:
09.11.2021

## Syventävä tieto

Voit myös laskea päivämäärän tiettyyn pisteeseen menneisyydessä tai tulevaisuudessa käyttämällä timedelta-metodia ja lisäämällä tai vähentämällä päiviä, viikkoja tai kuukausia tarvittavan määrän:

```Python
synnynpaiva = date(1990, 5, 20) # asetetaan päivämäärä muuttujaan

vuosi = timedelta(days=365) # lasketaan vuosi timedelta-metodilla

taysi_ika = synnynpaiva + 31*vuosi # lasketaan täysi-ikäisen ikä syntymäpäivään
print(taysi_ika.strftime("%d.%m.%Y")) # tulostetaan tuleva päivämäärä

kolmekymppinen = synnynpaiva + 40*vuosi # lasketaan kolmekymppisen ikä syntymäpäivään
print(kolmekymppinen.strftime("%d.%m.%Y")) # tulostetaan tuleva päivämäärä
```

Output:
20.05.2021
20.05.2030

Laskemalla päivämäärät Pythonilla voit olla varma tarkoista ja luotettavista tuloksista. Muista myös hyödyntää datetime-kirjaston muita metodeja ja muotoiluvaihtoehtoja päivämäärän tulostamiseen haluamassasi muodossa.

## Katso myös

- [datetime-kirjaston dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [Pythonin perusteet](https://www.w3schools.com/python/default.asp)
- [Timedelta-metodin käyttö](https://www.programiz.com/python-programming/datetime/strftime)