---
title:                "Tulevan tai menneen päivämäärän laskeminen"
aliases: - /fi/haskell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:38.235458-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Menneisyyden tai tulevaisuuden päivämäärän laskeminen tarkoittaa päivämäärän siirtämistä taakse- tai eteenpäin kalenterissa. Ohjelmoijat tekevät tätä mm. vanhentumispäivien, muistutusten tai aikataulutusten automatisoimiseksi.

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
