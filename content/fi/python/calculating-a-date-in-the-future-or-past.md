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

## Mitä ja miksi?
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on yksinkertaisesti tietyn päivämäärän lisäämistä tai vähentämistä tietty määrä päiviä. Tämä on yleinen tehtävä ohjelmoinnissa, sillä usein tarvitsemme tietoa tulevista tai menneistä päivämääristä esimerkiksi tapahtumien suunnittelussa tai laskutuksessa.

## Näin teet sen:
```Python
import datetime

# Lisätään 30 päivää nykyiseen päivämäärään
tuleva_päivä = datetime.date.today() + datetime.timedelta(days=30)
print(tuleva_päivä)

# Vähennetään 2 kuukautta nykyisestä päivämäärästä
mennyt_päivä = datetime.date.today() - datetime.timedelta(weeks=8)
print(mennyt_päivä)
```

**Lisätietoja:**
1. Historiallinen konteksti:
Päivämäärän laskeminen on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Alkuaikoina tämä tehtiin yleensä manuaalisesti laskemalla päiviä ja kuukausia, mutta nykyään siihen on olemassa valmiita työkaluja ja kirjastoja.

2. Vaihtoehtoiset menetelmät:
Voit myös käyttää `dateutil`-kirjastoa, joka tarjoaa lisää toiminnallisuutta päivämäärien laskemiseen, kuten huomioida erilaiset aikavyöhykkeet ja paikalliset lomat.

3. Toteutus:
Päivämäärien laskeminen perustuu ajanjakson lisäämiseen tai vähentämiseen nykyisestä päivämäärästä `timedelta`-luokan avulla. Tämä luokka mahdollistaa päivien, viikkojen, kuukausien ja vuosien lisäämisen tai vähentämisen.

## Katso myös:
- [Pythonin datetime moduuli](https://docs.python.org/3/library/datetime.html)
- [dateutil-kirjasto](https://dateutil.readthedocs.io/en/stable/)
- [Lyhyt video päivämäärien laskemisesta Pythonilla](https://www.youtube.com/watch?v=J4o7ln2ZDjo)