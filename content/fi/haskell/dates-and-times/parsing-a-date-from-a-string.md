---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases:
- /fi/haskell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:18.831743-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonosta päivämäärän jäsennys Haskellissa tarkoittaa tekstiesitysten muuntamista ohjelman käsiteltävissä olevaan rakenteelliseen muotoon. Tämä prosessi on perustavanlaatuinen sovelluksille, jotka käsittelevät kalenteritietoja, mahdollistaen toiminnot kuten kestojen laskennan, aikataulutuksen ja datan validoinnin.

## Kuinka:

Oletuksena Haskell tarjoaa perustyökalut päivämäärien jäsentämiseen, mutta kirjastojen kuten `time` käyttäminen ydintoiminnallisuuteen ja `date-parse` tai `time-parse` joustavampaan jäsentämiseen voi merkittävästi yksinkertaistaa tehtävää.

Ensimmäiseksi, varmista että sinulla on saatavilla `time`-kirjasto; se sisältyy usein GHC:hon, mutta jos sinun tarvitsee määritellä se riippuvuudeksi, lisää `time` projektiisi cabal-tiedostoon tai käytä `cabal install time` komentoa sen manuaaliseen asentamiseen.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Käyttämällä time-kirjastoa päivämäärän jäsennykseen standardimuodossa
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"
```

Esimerkin käyttö ja tuloste:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- Tuloste: Just 2023-03-31 22:00:00 UTC
```

Monimutkaisemmissa skenaarioissa, joissa sinun on käsiteltävä monia muotoja tai kieliäyttöalueita, kolmannen osapuolen kirjastot kuten `date-parse` voivat olla kätevämpiä:

Olettaen, että olet lisännyt `date-parse` riippuvuudet ja asentanut sen, tässä on miten saatat käyttää sitä:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- Jäsennetään päivämäärämerkkijono date-parse-kirjaston avulla, joka tukee useita muotoja
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

Esimerkin käyttö `date-parse` kanssa:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "huhtikuu 1, 2023"

-- Tuloste: Just 2023-04-01
```

Kukin esimerkki osoittaa peruslähestymistavan ottaa merkkijono ja muuntaa se käytettäväksi päivämääräobjektiksi Haskellissa. Valinta `time`-kirjaston sisäänrakennettujen funktioiden käytön ja kolmannen osapuolen ratkaisun kuten `date-parse` välillä riippuu sovelluksesi erityistarpeista, kuten tarvittavien syötemuotojen laajuudesta.
