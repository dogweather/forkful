---
date: 2024-01-20 17:31:38.235458-07:00
description: "How to: Haskellissa p\xE4iv\xE4m\xE4\xE4rien k\xE4sittelyyn voitiin\
  \ kukin k\xE4ytt\xE4\xE4 vanhempia kirjastoja kuten `Time` pakettia, mutta nyky\xE4\
  \xE4n `Data.Time` kirjasto on\u2026"
lastmod: '2024-04-05T21:53:58.198467-06:00'
model: gpt-4-1106-preview
summary: "Haskellissa p\xE4iv\xE4m\xE4\xE4rien k\xE4sittelyyn voitiin kukin k\xE4\
  ytt\xE4\xE4 vanhempia kirjastoja kuten `Time` pakettia, mutta nyky\xE4\xE4n `Data.Time`\
  \ kirjasto on suosiossa, kiitos sen monipuolisemman ja k\xE4ytt\xE4j\xE4yst\xE4\
  v\xE4llisen API:n."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to:
```Haskell
import Data.Time

-- Lisää päiviä nykyiseen päivämäärään
addDaysToCurrentDate :: Integer -> IO Day
addDaysToCurrentDate days = do
  today <- utctDay <$> getCurrentTime
  return $ addDays days today

-- Esimerkki: Lisää 10 päivää tästä päivästä
main :: IO ()
main = do
  futureDate <- addDaysToCurrentDate 10
  print futureDate

-- Tulostaisi esimerkiksi: 2023-04-15 jos tämä päivä on 2023-04-05
```
```Haskell
-- Vähennä päiviä nykyisestä päivämäärästä
subtractDaysFromCurrentDate :: Integer -> IO Day
subtractDaysFromCurrentDate days = do
  today <- utctDay <$> getCurrentTime
  return $ addDays (negate days) today

-- Esimerkki: Vähennä 10 päivää tästä päivästä
main :: IO ()
main = do
  pastDate <- subtractDaysFromCurrentDate 10
  print pastDate

-- Tulostaisi esimerkiksi: 2023-03-26 jos tämä päivä on 2023-04-05
```

## Deep Dive
Haskellissa päivämäärien käsittelyyn voitiin kukin käyttää vanhempia kirjastoja kuten `Time` pakettia, mutta nykyään `Data.Time` kirjasto on suosiossa, kiitos sen monipuolisemman ja käyttäjäystävällisen API:n. `Data.Time` tarjoaa funktioita kuten `addDays`, joka helpottaa päivämäärien laskentaa.

Vaihtoehtoisesti voi hyödyntää myös muita kirjastopaketteja, kuten `time-lens`, joka tarjoaa funktionaalisempia tapoja ajan manipuloimiseen. Kuitenkin `Data.Time` on usein riittävä useimpiin tarpeisiin ja se on hyvin dokumentoitu.

Kun lasketaan tulevaisuuden tai menneisyyden päivämääriä, tärkeää on huomioida aikavyöhykkeet ja kesäaikaan siirtymiset. Tämä voi vaikuttaa lopputulokseen, etenkin jos ajanhetkiin liittyy kellonaikoja.

## See Also
- Haskell `Data.Time` –dokumentaatio: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Haskell-päivämäärä- ja aikakirjastojen vertailu: https://wiki.haskell.org/Library/Time
- Keskustelua Haskellin aikakirjastoista: https://www.reddit.com/r/haskell/comments/3q9oxp/best_library_for_dealing_with_dates_and_times/
