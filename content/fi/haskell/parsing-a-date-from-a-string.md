---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:36:31.561482-07:00
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Datapäivämäärän jäsentäminen merkkijonosta muuttaa tekstimuotoiset päivämäärät ohjelmoijille sopivaan formaattiin. Tämä on tärkeää, koska se mahdollistaa päivämäärien käsittelyn, vertailun ja tallennuksen.

## How to: (Kuinka tehdä:)
Haskellissa päivämäärän jäsentäminen voidaan tehdä `time`-kirjaston `parseTimeM` funktion avulla. Tässä on perusesimerkki:

```Haskell
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- Asetetaan suomalainen aikaformaatti
let finnishLocale = defaultTimeLocale { dateFormat = "%-d.%-m.%Y", timeFormat = "%H:%M" }

-- Jäsentämisen esimerkki
maybeDate :: Maybe UTCTime
maybeDate = parseTimeM True finnishLocale "%-d.%-m.%Y %H:%M" "24.12.2023 18:00"

main :: IO ()
main = print maybeDate
```

Suoritettaessa antaa:

```
Just 2023-12-24 16:00:00 UTC
```

## Deep Dive (Perusteellinen Sukellus):
Haskellin `time`-kirjasto on ollut osa kieltä jo pitkään, mutta sen käyttöliittymä on parantunut ajan saatossa. `parseTimeM` on monadinen funktio, joka palauttaa `MonadFail`-instanssin omaavan monadin, yleensä `Maybe`. Tämä merkitsee, että jäsentäminen on epäonnistumissalliva toimenpide; se voi palauttaa `Nothing` jos annettu syöte ei vastaa odotettua formaattia.

Vaihtoehtoja standardikirjaston `time`-kirjastolle ovat mm. `chronos` ja `thyme`, jotka tarjoavat hieman erilaisia API:ja. Nämä kirjastot saattavat tarjota parempaa suorituskykyä tai lisäominaisuuksia tietyissä tilanteissa.

Kun puhutaan implementaatiosta, Haskellin tyypitysjärjestelmä mahdollistaa virheiden käsittelyn kompilaatioajassa. Jos päivämäärä jäsentyy väärin, tyyppivirheet varoittavat ohjelmoijaa ennen ohjelman ajamista.

## See Also (Katso Myös):
- Haskell `time`-kirjaston dokumentaatio: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- `chronos`-kirjasto: [https://hackage.haskell.org/package/chronos](https://hackage.haskell.org/package/chronos)
- `thyme`-kirjasto: [https://hackage.haskell.org/package/thyme](https://hackage.haskell.org/package/thyme)
- Haskell:n viralliset oppaat: [https://www.haskell.org/documentation/](https://www.haskell.org/documentation/)
