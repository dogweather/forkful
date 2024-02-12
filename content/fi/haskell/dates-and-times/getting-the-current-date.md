---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- /fi/haskell/getting-the-current-date.md
date:                  2024-02-03T19:09:36.707747-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän noutaminen Haskellissa käsittää järjestelmän nykyisen ajan hankkimisen ja sen muuntamisen luettavaan päivämäärämuotoon. Ohjelmoijat tekevät tämän suorittaakseen toimintoja päivämäärän perusteella, kuten lokitusta, tehtävien ajoitusta tai tapahtumien aikaleimausta sovelluksissa.

## Kuinka:
Haskellin vakiokirjasto, `base`, tarjoaa `Data.Time` -moduulin, joka tarjoaa toiminnallisuuden työskentelyyn päivämäärien ja aikojen kanssa. Tässä on miten sitä käytetään nykyisen päivämäärän saamiseen:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Esimerkkitulostus:
```
2023-04-12
```

Lisäjoustavuuden saamiseksi, kuten päivämäärän muotoiluun tai työskentelyyn eri aikavyöhykkeiden kanssa, `time`-kirjasto on korvaamaton. Tässä on miten saatat muotoilla nykyisen päivämäärän:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Tämä tulostaa nykyisen päivämäärän `YYYY-MM-DD`-muodossa, mukautettuna paikalliseen aikavyöhykkeeseen.

Lisäksi kolmannen osapuolen kirjastotuen saamiseksi, `time`-kirjastoa suositellaan erittäin ja sitä käytetään usein Haskell-yhteisössä sen laajojen päivämäärän ja ajan käsittelykykyjen vuoksi. Yllä olevat esimerkit hyödyntävät tätä kirjastoa.

Jos tarvitset kattavampaa päivämäärän käsittelyä, mukaan lukien jäsentäminen merkkijonoista tai aritmeettiset operaatiot päivämäärien ja aikojen kanssa, `Data.Time`-moduulin lisätoimintoihin tutustuminen on hyödyllistä.
