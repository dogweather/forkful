---
title:                "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Haskell ohjelmointikielellä on monia hyödyllisiä ominaisuuksia, ja yksi näistä on kyky saada tämänhetkinen päivämäärä. Tämä on hyödyllistä esimerkiksi kun haluat tallentaa tietoja ajan mukaisesti tai luoda dynaamisia ohjelmia, jotka käsittelevät päivämääriä. Seuraavassa kerromme, miten voit helposti saada tämänhetkisen päivämäärän Haskellilla.

## Kuinka

Haskellissa on valmiiksi sisäänrakennettu funktio `getCurrentTime`, joka palauttaa tämänhetkisen ajan `Data.Time.Clock` -moduulissa. Voit käyttää tätä funktiota haluamassasi ohjelmassa "Data.Time.Clock" -moduulin tuomiseen ja seuraavalla tavalla:

```Haskell
import Data.Time.Clock

currentDate <- getCurrentTime
```

Tämä luo muuttujan `currentDate`, johon tallennetaan tämänhetkinen aika. Voit muuttaa tämän ajan haluamasi muotoon muiden `Data.Time` -moduulin funktioiden avulla, kuten `formatTime`.

```Haskell
formatTime defaultTimeLocale "%d-%m-%Y" currentDate
```

Tämä tulostaa päivämäärän muodossa "DD-MM-YYYY". Voit myös käyttää muita muotoilumalleja oman tarpeesi mukaan.

## Syvemmällä

Haskellin `getCurrentTime` -funktio perustuu alhaisemmalla tasolla olevaan `Data.Time.Clock.POSIX` -moduuliin, joka palauttaa UNIX-aikaleiman sekunteina. Tämä aikaleima ilmaisee ajan kulun tietystä päivästä tammikuuta 1970. Tämä tapahtuu tarkan ajan ilmoittamiseksi ja käytetään usein teknisissä sovelluksissa.

Voit käyttää `posixSecondsToUTCTime` -funktiota muuntaaksesi POSIX-aikaleiman UTC-aikaleimaksi ja sitten käyttää `formatTime` -funktiota muotoillaksesi sen haluamallasi tavalla. Tämä tieto voi olla hyödyllinen, jos sinun täytyy käsitellä aikaa tarkemmin.

## Katso myös (See Also)

- [Haskellin ajan käsittely](https://www.haskell.org/hoogle/?hoogle=time)
- [Haskellin aikaleimojen muuntaminen](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html#v:formatTime)