---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen"
html_title:           "Haskell: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihminen haluaisi laskea päivämäärän tulevaisuudessa tai menneisyydessä? Yksi mahdollinen syy on esimerkiksi tapahtuman suunnittelu tulevalle päivälle tai takautuvien laskujen tekeminen menneisyydestä.

## Miten

Laskemalla päiviä tulevaisuuteen tai menneisyyteen Haskellilla on helppoa. Tarvitsemme vain kaksi tietotyyppiä: `Day` ja `DiffTime`.

```Haskell
import Data.Time

tulevaPaiva = addDays 7 (utctDay getCurrentTime)
putStrLn $ "Päivämäärä tasan viikon päästä: " ++ show tulevaPaiva

menneisyys = addDays (-30) (utctDay getCurrentTime)
putStrLn $ "Päivämäärä tasan kuukausi sitten: " ++ show menneisyys
```

Tässä näemme, että voimme lisätä tai vähentää päiviä tietystä päivästä käyttämällä `addDays` -funktiota. Marina `utctDay getCurrentTime` käytetään nykyisen päivämäärän hakemiseen ja tulostetaan halutulla tavalla.

## Syväsukellus

Haskelissa päivämäärätyyppejä käsitellään `Data.Time` -kirjastossa. Tämä kirjasto tarjoaa monia hyödyllisiä toimintoja päivämäärien ja aikojen laskemiseen ja muokkaamiseen.

On myös syytä huomata, että päivämäärien käsittely Haskelissa on erityisen tarkkaa ja yksiselitteistä verrattuna muihin ohjelmointikieliin. Esimerkiksi tiettyä päivämäärää vastaava arvo on aina sama, riippumatta aikavyöhykkeestä tai muista tekijöistä.

## Katso myös

- [Data.Time-kirjasto Haskellin dokumentaatiossa](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskelldoc - yhteisöllinen dokumentaatio Haskellista](https://hackage.haskell.org/package/haskelldoc)