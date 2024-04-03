---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:36.707747-07:00
description: "Kuinka: Haskellin vakiokirjasto, `base`, tarjoaa `Data.Time` -moduulin,\
  \ joka tarjoaa toiminnallisuuden ty\xF6skentelyyn p\xE4iv\xE4m\xE4\xE4rien ja aikojen\
  \ kanssa. T\xE4ss\xE4\u2026"
lastmod: '2024-03-13T22:44:56.625237-06:00'
model: gpt-4-0125-preview
summary: "Haskellin vakiokirjasto, `base`, tarjoaa `Data.Time` -moduulin, joka tarjoaa\
  \ toiminnallisuuden ty\xF6skentelyyn p\xE4iv\xE4m\xE4\xE4rien ja aikojen kanssa."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
