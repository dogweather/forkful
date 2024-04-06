---
date: 2024-01-20 17:36:59.458854-07:00
description: "How to: - Kuinka tehd\xE4: P\xE4iv\xE4m\xE4\xE4r\xE4t merkkijonoina\
  \ ovat olleet k\xE4yt\xF6ss\xE4 tietokoneiden alkuaikojen j\xE4lkeen, jolloin ihmiset\
  \ alkoivat tarvita tapoja\u2026"
lastmod: '2024-04-05T22:51:10.779770-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4: P\xE4iv\xE4m\xE4\xE4r\xE4t merkkijonoina ovat olleet\
  \ k\xE4yt\xF6ss\xE4 tietokoneiden alkuaikojen j\xE4lkeen, jolloin ihmiset alkoivat\
  \ tarvita tapoja luettavasti tallentaa ja esitt\xE4\xE4 ajanhetki\xE4. Historiallisesti\
  \ C ja C++ ovat vaikuttaneet muiden kielien p\xE4iv\xE4m\xE4\xE4r\xE4k\xE4sittelyyn.\
  \ Haskell k\xE4ytt\xE4\xE4 `Data.Time` kirjastoa p\xE4iv\xE4m\xE4\xE4r\xE4n k\xE4\
  sittelyyn. `formatTime` funktio mahdollistaa p\xE4iv\xE4m\xE4\xE4r\xE4n muuntamisen\
  \ monenlaisiin formaatteihin. Muotoilujono, esim. `\"%Y-%m-%d\"`, m\xE4\xE4rittelee\
  \ tulosteen muodon. `defaultTimeLocale` m\xE4\xE4r\xE4\xE4 kulttuurikohtaiset asetukset,\
  \ kuten viikonp\xE4ivien ja kuukausien nimet. Vaihtoehtoisesti, Haskell tarjoaa\
  \ my\xF6s `time` kirjaston, joka sis\xE4lt\xE4\xE4 vanhemmat funktiot, kuten `show`\
  \ ajan esitt\xE4miseen, mutta ne ovat v\xE4hemm\xE4n joustavia. Suoritusyksityiskohdat\
  \ liittyv\xE4t siihen, miten Haskell k\xE4sittelee aikaan liittyvi\xE4 tietotyyppej\xE4\
  \ ja kuinka `formatTime` funktio purkaa muotoilujonon muuntaakseen p\xE4iv\xE4m\xE4\
  \xE4r\xE4arvon merkkijonoksi. Joustavuuden ja kansainv\xE4listymisen my\xF6t\xE4\
  \ on tullut tarve tukea erilaisia kalentereita ja kulttuurisidonnaisia aikamuotoja."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

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
