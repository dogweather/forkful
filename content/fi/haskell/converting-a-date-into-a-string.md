---
title:                "Haskell: Muunna päivämäärä merkkijonoksi."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarve muuttaa päivämäärä merkkijonoksi, esimerkiksi käyttäjälle näytettäväksi tai tietokantaan tallennettavaksi. Tässä blogikirjoituksessa käymme läpi, miten tämä tehdään Haskell-ohjelmointikielellä.

## Kuinka

Mikäli käytät jonkinlaista päivämäärätyyppiä, esimerkiksi `Day`, voit käyttää `Data.Time.Format` -kirjaston `formatTime`-funktiota muuttaaksesi päivämäärän merkkijonoksi. Esimerkiksi:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

pvm :: Day
pvm = fromGregorian 2021 9 23

muunnettuPvm :: String
muunnettuPvm = formatTime defaultTimeLocale "%A %d.%m.%Y" pvm

main :: IO ()
main = putStrLn muunnettuPvm
```

Tämä koodi tulostaa päivämäärän muodossa "torstai 23.09.2021". Huomaa, että `formatTime`-funktion ensimmäinen parametri on `defaultTimeLocale`, joka määrittää päivämäärän muotoilun kielen ja formaatin. Voit käyttää myös muita esimerkiksi `en_US` tai `fi_FI` kieli-ja formaattivaihtoehtoja.

Jos haluat muuttaa vain päivämäärän ilman kellonaikaa, voit käyttää `formatDay`-funktiota:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

pvm :: Day
pvm = fromGregorian 2021 9 23

muunnettuPvm :: String
muunnettuPvm = formatDay defaultTimeLocale "%A %d.%m.%Y" pvm

main :: IO ()
main = putStrLn muunnettuPvm
```

Tämä koodi tulostaa päivämäärän muodossa "23.09.2021".

## Syvempää tietoa

`Data.Time.Format` -kirjastossa on myös muita hyödyllisiä funktioita, kuten `parseTimeM`, jolla voit muuttaa merkkijonon päivämääräksi. Lisäksi voit määrittää omia päivämääräformaatteja käyttäen `Data.Time.Format.ISO8601` -kirjastoa.

## Katso myös

- [Data.Time.Format - dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell-tutoriaali - päivämäärän muotoilu](https://www.haskell.org/tutorial/numbers.html#date-and-time)