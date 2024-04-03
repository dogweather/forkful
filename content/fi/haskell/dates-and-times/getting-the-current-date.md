---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:36.707747-07:00
description: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n noutaminen Haskellissa k\xE4sitt\xE4\
  \xE4 j\xE4rjestelm\xE4n nykyisen ajan hankkimisen ja sen muuntamisen luettavaan\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4muotoon. Ohjelmoijat\u2026"
lastmod: '2024-03-13T22:44:56.625237-06:00'
model: gpt-4-0125-preview
summary: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n noutaminen Haskellissa k\xE4sitt\xE4\
  \xE4 j\xE4rjestelm\xE4n nykyisen ajan hankkimisen ja sen muuntamisen luettavaan\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4muotoon."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
