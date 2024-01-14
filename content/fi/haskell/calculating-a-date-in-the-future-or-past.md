---
title:    "Haskell: Päivämäärän laskeminen tulevaisuudessa tai menneessä"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi laskea päivämäärän tulevaisuutta tai menneisyyttä?

Monissa ohjelmoinnin projekteissa tarvitaan kykyä laskea päivämäärät tulevaisuudessa tai menneisyydessä. Tämä voi olla hyödyllistä esimerkiksi laskutusjärjestelmässä, jossa tarvitaan tieto tulevista maksupäivistä. Tällaisia laskelmia voi myös tarvita vaikkapa varausjärjestelmässä tai tapahtumahallinnassa.

## Miten

Onneksi Haskellissa on helppo laskea päivämääriä tulevaisuudessa tai menneisyydessä. Tämä onnistuu käyttämällä `Data.Time`-kirjastoa ja sen `addDays`-funktiota.

Esimerkiksi, jos haluat laskea päivämäärän 10 päivää tulevaisuuteen, voit käyttää seuraavaa koodia:

```Haskell
import Data.Time

tanaan <- getCurrentTime
tulevaisuus <- return $ addDays 10 tanaan
print tulevaisuus
```

Tämä koodi ensin hakee nykyisen päivämäärän ja tallentaa sen muuttujaan `tanaan`. Sitten se laskee 10 päivää nykyisestä päivämäärästä eteenpäin käyttäen `addDays`-funktiota ja tallentaa tulevan päivämäärän muuttujaan `tulevaisuus`. Lopuksi se tulostaa tulevan päivämäärän konsoliin.

Tämän esimerkin tuloste voisi olla esimerkiksi `2021-11-20 00:00:00 +0200`, riippuen siitä, mihin päivään sitä ajetaan.

## Syvähdyssukellus

`Data.Time`-kirjastossa on muitakin hyödyllisiä funktioita päivämäärien laskemiseen. Yksi tällainen funktio on `addGregorianMonthsClip`, joka laskee päivämääriä kuukausien tai vuosien päähän. Tämä funktio kuitenkin klippaa päivämäärän, eli jos laskettu päivämäärä ei ole oikea päivämäärä kyseisessä kuukaudessa (esim. helmikuussa ei ole 31. päivää), se asettaa sen automaattisesti viimeiseksi päivämääräksi kyseisessä kuukaudessa.

Toinen hyödyllinen funktio on `diffDays`, jolla voi laskea päivien välisen eron kahden päivämäärän välillä. Tämän avulla voi esimerkiksi laskea, kuinka monta päivää on seuraavaan laskutuspäivään tai kuinka monta päivää on syntymäpäivääsi.

## Katso myös

- [Haskellin virallinen dokumentaatio `Data.Time`-kirjastosta](https://www.stackage.org/haddock/lts-17.19/time-1.9.3/Data-Time.html)
- [Ohjelmoijan päivämäärän laskeminen tulevaisuudessa tai menneisyydessä käyttäen JavaScriptiä](https://x-team.com/blog/future-and-past-date-calculation-in-javascript/) (englanniksi)
- [Päivien laskeminen Excelissä](https://support.microsoft.com/en-us/office/calculate-the-difference-between-two-dates-8235e7c9-b430-44ca-9425-46100a162f38) (englanniksi)