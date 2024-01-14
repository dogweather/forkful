---
title:    "Python: Nykyisen päivämäärän hankkiminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Python on suosittu ohjelmointikieli monesta syystä, yksi niistä on sen helppokäyttöisyys ja monipuolisuus. Yksi tapa hyödyntää Pythonia on saada nykyinen päivämäärä koodilla. Tässä blogikirjoituksessa tarkastelemme, miten voit saada nykyisen päivämäärän Pythonilla ja miksi se voi olla hyödyllistä.

## Miten

Käytä ```datetime``` -kirjastoa saadaksesi nykyisen päivämäärän Pythonilla:
```Python
from datetime import date
tämä_päivä = date.today()
print(tämä_päivä)
```

Tämän koodin tulostus on muodossa ```vvvv-kk-pp```, esimerkiksi ```2021-07-22```. Jos haluat näyttää päivämäärän eri muodossa, voit käyttää ```strftime()``` -funktiota ```datetime``` -kirjastossa:
```Python
tämä_päivä = date.today().strftime("%d/%m/%Y")
print(tämä_päivä)
```

Tämä tulostaa päivämäärän muodossa ```pp/kk/vvvv```, esimerkiksi ```22/07/2021```. Voit muuttaa tulostetta haluamallasi tavalla käyttämällä eri formaatteja ```strftime()``` -funktiolla.

## Syvällinen sukellus

Nykyisen päivämäärän saaminen Pythonilla voi olla hyödyllistä monissa eri tilanteissa, kuten aikaleimojen tallentamisessa, päiväyslaskureiden laskemisessa tai raporttien luomisessa.

Voit myös saada tarkemman ajan lisäämällä aikaan liittyvät tiedot ```datetime``` -kirjastolla:
```Python
from datetime import datetime
tämä_aika = datetime.now()
print(tämä_aika)
```

Tulostus näyttää nyt sekä päivämäärän että kellonajan: ```vvvv-kk-pp hh:mm:ss.µµµµµ```, esimerkiksi ```2021-07-22 18:25:45.548603```.

## Katso myös

- [Pythonin virallinen dokumentaatio datetime-kirjastosta](https://docs.python.org/3/library/datetime.html)
- [Python DateTime Objects-tutorial](https://www.programiz.com/python-programming/datetime)
- [W3Schools-ohjeet Python DateTime-kirjastoon](https://www.w3schools.com/python/python_datetime.asp)