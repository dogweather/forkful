---
title:                "Muuntaa päivämäärä merkkijonoksi"
html_title:           "Haskell: Muuntaa päivämäärä merkkijonoksi"
simple_title:         "Muuntaa päivämäärä merkkijonoksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Haluaisitko oppia, kuinka muuttaa päivämäärä merkkijonoksi Haskell-ohjelmoinnissa? Ehkä haluat tulostaa päivämäärän käyttäjälle ymmärrettävässä muodossa tai tallentaa sen tietokantaan. Tämä artikkeli auttaa sinua ymmärtämään tämän tärkeän tehtävän Haskellissa.

## Kuinka

Haskellissa päivämäärän muuntaminen merkkijonoksi tapahtuu käyttämällä `formatTime` -funktiota `time` -kirjastosta. Voit aloittaa luomalla päivämäärämuuttujan ja määrittämällä haluamasi muodon, esimerkiksi `%d.%m.%Y` näyttää päivämäärän muodossa päivä-kuukausi-vuosi. Sitten voit kutsua `formatTime` ja antaa parametreiksi päivämäärämuuttujan ja haluamasi muodon, kuten seuraavassa esimerkissä näkyy:

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  currentTime <- getCurrentTime
  let date = "%d.%m.%Y"
  let formattedDate = formatTime defaultTimeLocale date currentTime
  putStrLn formattedDate

```

Tämä tulostaa nykyisen päivämäärän muodossa `01.03.2021` (olettaen, että tänään on 1. maaliskuuta 2021).

## Syvä Sukellus

On tärkeää mainita, että `formatTime` -funktio käyttää `locale` -parametria määrittämään kieliasetuksia ja ajan muotoja. Tämä tarkoittaa sitä, että voit mukauttaa päivämäärän muotoa eri kielille ja kulttuureille muuttamalla `locale`-parametria. Voit myös käyttää muita muotoilumerkkejä, kuten `%H` tunnin esittämiseen tai `%M` minuuttien esittämiseen. Voit löytää täyden listan muotoilumerkkeistä `time` -kirjaston dokumentaatiosta.

Voit myös muuntaa päivämäärän merkkijonosta takaisin päivämääräksi käyttämällä `parseTime` -funktiota. Tämä on hyödyllistä, kun haluat lukea päivämäärän tiedostosta tai tietokannasta ja käsitellä sitä Haskellissa.

## Katso myös

- [Haskellin `time` -kirjaston dokumentaatio](https://hackage.haskell.org/package/time)
- [Haskellin opetusohjelmat ja dokumentaatio](https://www.haskell.org/documentation/)