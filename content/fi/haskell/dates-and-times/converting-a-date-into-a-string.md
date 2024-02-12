---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:36:59.458854-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Muuttamalla päivämäärän merkkijonoksi voimme näyttää sen helposti ymmärrettävässä muodossa. Ohjelmoijat tekevät tämän, jotta päivämäärät sopivat käyttöliittymiin ja logeihin ihmisten luettaviksi.

## How to: - Kuinka tehdä:
```Haskell
import Data.Time

-- Oletetaan, että meillä on päivämäärä muuttujassa 'date'
date :: UTCTime
date = UTCTime (fromGregorian 2023 4 7) (secondsToDiffTime 0)

-- Muuntaa päivämäärän merkkijonoksi
dateToString :: UTCTime -> String
dateToString = formatTime defaultTimeLocale "%Y-%m-%d"

-- Esimerkki käytöstä
main :: IO ()
main = putStrLn $ dateToString date
```

Output:
```
2023-04-07
```

## Deep Dive - Syväsukellus
Päivämäärät merkkijonoina ovat olleet käytössä tietokoneiden alkuaikojen jälkeen, jolloin ihmiset alkoivat tarvita tapoja luettavasti tallentaa ja esittää ajanhetkiä. Historiallisesti C ja C++ ovat vaikuttaneet muiden kielien päivämääräkäsittelyyn. 

Haskell käyttää `Data.Time` kirjastoa päivämäärän käsittelyyn. `formatTime` funktio mahdollistaa päivämäärän muuntamisen monenlaisiin formaatteihin. Muotoilujono, esim. `"%Y-%m-%d"`, määrittelee tulosteen muodon. `defaultTimeLocale` määrää kulttuurikohtaiset asetukset, kuten viikonpäivien ja kuukausien nimet.

Vaihtoehtoisesti, Haskell tarjoaa myös `time` kirjaston, joka sisältää vanhemmat funktiot, kuten `show` ajan esittämiseen, mutta ne ovat vähemmän joustavia.

Suoritusyksityiskohdat liittyvät siihen, miten Haskell käsittelee aikaan liittyviä tietotyyppejä ja kuinka `formatTime` funktio purkaa muotoilujonon muuntaakseen päivämääräarvon merkkijonoksi. Joustavuuden ja kansainvälistymisen myötä on tullut tarve tukea erilaisia kalentereita ja kulttuurisidonnaisia aikamuotoja.

## See Also - Katso Myös
- [Haskell.org Documentation](https://www.haskell.org/documentation/) – Haskellin virallinen dokumentaatio.
- [Data.Time Library on Hackage](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html) – Tarkemmat tiedot `Data.Time` kirjastosta.
