---
title:                "Tänään olevan päivämäärän haku"
html_title:           "Haskell: Tänään olevan päivämäärän haku"
simple_title:         "Tänään olevan päivämäärän haku"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää nykyisen päivämäärän? No, ehkä haluat tehdä pienet joululahjalistat tai yksinkertaisesti vain pitää kirjaa ajasta ohjelmassasi. Ei hätää, tässä on helppo tapa selvittää nykyinen päivämäärä käyttäen Haskellia!

## Miten

*Haskellin mukavat minifunktiot takaavat, että ```Data.Time``` on helppo tapa saada nykyinen päivämäärä:*

```Haskell
import Data.Time

main = do
  day <- getCurrentDay
  print day
  -- Tulostaa esimerkiksi: 2021-12-02
```

Voit myös tulostaa päivämäärän haluamassasi muodossa käyttämällä funktiota ```formatTime``` ja antamalla sille haluamasi muodon merkkijonona:

```Haskell
import Data.Time
import System.Locale (TimeLocale, defaultTimeLocale)

main = do
  day <- getCurrentDay
  print (formatTime defaultTimeLocale "%A %B %d, %Y" day)
  -- Tulostaa esimerkiksi: torstai joulukuu 2, 2021
```
Voit vaihtaa päivämäärän kieltä käyttämällä ```Data.Time.Locale``` -moduulia ja antamalla haluamasi kielikoodin ```formatTime``` -funktion toisena argumenttina.

## Syväsukellus

```Data.Time``` sisältää monia hyödyllisiä funktioita, mutta jos haluat syvällisempää tietoa päivämäärän hallinnasta, voit tutustua ```Clock``` -moduuliin.

Esimerkiksi, voit käyttää ```getCurrentTime``` -funktiota ```Clock``` -moduulista saadaksesi myös nykyisen ajan lisäksi päivämäärän:

```Haskell
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (toGregorian)

main = do
  dateTime <- getCurrentTime
  let (year, month, day) = toGregorian (utctDay dateTime)
  putStrLn (show day ++ "/" ++ show month ++ "/" ++ show year)
  -- Tulostaa esimerkiksi: 2/12/2021 
```

## Katso myös

- [Haskellin virallinen verkkosivusto](https://www.haskell.org/)
- [Haskillin dokumentaatio](https://www.haskell.org/documentation/)