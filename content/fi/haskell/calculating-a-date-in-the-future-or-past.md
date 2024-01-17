---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Haskell: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on tapa laskea päiviä tai aikoja tietystä lähtöpäivästä. Ohjelmoijat voivat tehdä tämän esimerkiksi tarkastellakseen tiettyjen tapahtumien tai tapaamisten päivämäärää tai yksinkertaisesti vain saadakseen tarkempaa tietoa ajasta.

## Näin teet sen:
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on helppoa käyttäen Haskellin ```addDays``` ja ```addUTCTime``` -funktioita. Alla on esimerkkejä ja tulostus näistä funktioista.

```haskell
addDays 10 (fromGregorian 2021 10 10)
-- Output: 2021-10-20

addUTCTime 3600 (UTCTime (fromGregorian 2021 10 10) (secondsToDiffTime 0))
-- Output: 2021-10-10 01:00:00 UTC
```

## Syvempi sukellus:
Päivämäärän laskenta tulevaisuuteen tai menneisyyteen perustuu julian-päivien käsitteeseen, joka tarkoittaa päivien lukumäärää 1.1.4713 eKr. lähtien. Historiallisesti julian-päiviä käytettiin päivämäärien laskemiseen, mutta nykyään yleisemmin käytetty Gregoriaaninen kalenteri perustuu tarkempaan laskentatapaan. Haskellissa on myös muita tapoja laskea päivämääriä, kuten käyttämällä ```diffDays``` ja ```diffUTCTime``` -funktioita.

## Katso myös:
- [DateTime-kirjasto Haskellissa](https://hackage.haskell.org/package/datetime)
- [Haskellin aikamoduulit](https://www.haskell.org/onlinereport/libraries/time.html)