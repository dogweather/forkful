---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:30:39.440942-07:00
description: "Kuinka: Elmin `Time`-moduuli ja `justinmimbs/time-extra`-paketti mahdollistavat\
  \ p\xE4iv\xE4m\xE4\xE4rien k\xE4sittelyn helposti."
lastmod: '2024-04-04T00:26:52.683740-06:00'
model: gpt-4-0125-preview
summary: "Elmin `Time`-moduuli ja `justinmimbs/time-extra`-paketti mahdollistavat\
  \ p\xE4iv\xE4m\xE4\xE4rien k\xE4sittelyn helposti."
title: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Kuinka:
Elmin `Time`-moduuli ja `justinmimbs/time-extra`-paketti mahdollistavat päivämäärien käsittelyn helposti.

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: lisättävien (negatiivinen vähentää) päivien määrä
-- @fromDate: lähtöpäivämäärä Posix-muodossa

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- Käyttö
-- Älä unohda, että Elm laskee ajan millisekunteina Unix-epookista lähtien.

sampleDate = Time.millisToPosix 1580515200000  -- 1. helmikuuta 2020 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- Lisää 10 päivää
pastDate = calculateDate -15 sampleDate        -- Vähentää 15 päivää

-- esimerkkitulosteet:
-- futureDate -> 1581552000000  -- 12. helmikuuta 2020 00:00:00 UTC
-- pastDate -> 1580006400000    -- 17. tammikuuta 2020 00:00:00 UTC
```

## Syväsukellus
Aikaisemmin päivämäärien käsittely ohjelmoinnissa oli tuskaista. Eri järjestelmät, formaatit ja aikavyöhykkeet tuottivat kaikille päänsärkyä. Elmin `Time`-moduuli, joka perustuu Unixin aikajärjestelmään (millisekunteina vuodesta 1970), standardoi tämän. Paketti `justinmimbs/time-extra` yksinkertaistaa edelleen päivämäärien käsittelytoimenpiteitä, kuten päivien lisäämistä tai vähentämistä.

Vaihtoehtoja? Muilla kielillä on omat kirjastonsa, kuten Pythonin `datetime` tai JavaScriptin `Date`. Mutta Elmin lähestymistapa tarjoaa vahvan tyypityksen ja puhtauden, mikä vähentää bugeja.

Päivien lisäämisen tai vähentämisen lisäksi voit myös työskennellä kuukausien, vuosien tai jopa tuntien ja minuuttien kanssa. Elm:ssä ja paketeissa, kuten `time-extra`, olevat funktiot keskittyvät muuttumattomuuteen ja puhtaisiin funktioihin—tämä tarkoittaa, että sivuvaikutuksia ei ole. Kun lasket uuden päivämäärän, alkuperäinen pysyy muuttumattomana.

## Katso myös
- Elm `Time` moduuli: https://package.elm-lang.org/packages/elm/time/latest/
- `justinmimbs/time-extra`-paketti: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Elm-oppaassa aika: https://guide.elm-lang.org/effects/time.html
