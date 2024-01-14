---
title:                "Haskell: Muuttamassa päivämäärää merkkijonoksi"
simple_title:         "Muuttamassa päivämäärää merkkijonoksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärän muuttaminen merkkijonoksi voi olla hyödyllistä esimerkiksi tietokannassa tallennettaessa tai käyttäjälle näytettäessä. Tällöin tieto on selvää ja helppolukuista.

## Miten: Esimerkki koodista ja tulosteesta

```Haskell
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar

päivämäärä :: Day
päivämäärä = fromGregorian 2021 4 15

muunnaMerkkijonoksi :: Day -> String
muunnaMerkkijonoksi päivämäärä = formatTime defaultTimeLocale "%d.%m.%Y" päivämäärä

main :: IO ()
main = putStrLn (muunnaMerkkijonoksi päivämäärä)
```

Tuloste: 15.04.2021

## Syvemmälle: Päivämäärän muuttaminen merkkijonoksi

Päivämäärän muuttaminen merkkijonoksi tapahtuu muuttamalla ensin päivämäärä Data.Time.Format-paketin DateTime muotoon. Tämän jälkeen voidaan käyttää formatTime-funktiota, joka ottaa parametreina mm. halutun merkintätavan ja DateTime-muotoisen päivämäärän. Tämän jälkeen tuloksena saadaan haluttu päivämäärä merkkijonona.

## Katso myös

- [Data.Time.Format-paketti](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Data.Time.Clock-paketti](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [Data.Time.Calendar-paketti](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)