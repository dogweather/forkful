---
date: 2024-01-20 17:32:39.584173-07:00
description: "Kuinka: Elmiss\xE4 p\xE4iv\xE4m\xE4\xE4rien vertailu ei ole yht\xE4\
  \ suoraviivaista kuin joissain muissa kieliss\xE4, johtuen kielisuunnittelusta,\
  \ joka suosii puhtautta ja\u2026"
lastmod: '2024-04-05T21:53:58.066687-06:00'
model: gpt-4-1106-preview
summary: "Elmiss\xE4 p\xE4iv\xE4m\xE4\xE4rien vertailu ei ole yht\xE4 suoraviivaista\
  \ kuin joissain muissa kieliss\xE4, johtuen kielisuunnittelusta, joka suosii puhtautta\
  \ ja turvallisuutta."
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertailu"
weight: 27
---

## Kuinka:
```Elm
import Time exposing (Posix)
import Date

-- Päivämäärän luominen
date1 : Date.Date
date1 = Date.fromIsoString "2023-03-21" |> Result.withDefault Date.zero -- HUOM: Käsittele Result paremmin todellisissa projekteissa

date2 : Date.Date
date2 = Date.fromIsoString "2023-03-25" |> Result.withDefault Date.zero -- HUOM: Käsittele Result paremmin todellisissa projekteissa

-- Päivämäärän vertailu
compareDates : Date.Date -> Date.Date -> Basics.Order
compareDates d1 d2 =
    Date.compare d1 d2

-- Esimerkkitulostus
compareExample : String
compareExample =
    case compareDates date1 date2 of
        LT -> "Ensimmäinen päivämäärä on aikaisempi."
        EQ -> "Päivämäärät ovat samat."
        GT -> "Toinen päivämäärä on aikaisempi."

-- Tulostaa: "Ensimmäinen päivämäärä on aikaisempi."
```

## Syväsukellus
Elmissä päivämäärien vertailu ei ole yhtä suoraviivaista kuin joissain muissa kielissä, johtuen kielisuunnittelusta, joka suosii puhtautta ja turvallisuutta. Date-moduuli esimerkiksi palauttaa `Result`-tyypin, joka pakottaa käsittelyyn virhetilanteet. Historiallisesti Elm on kehittynyt sellaiseksi, jossa "time-travel debugger" on ollut ainutlaatuinen ominaisuus, mikä vaikutti siihen, miten aikaa ja päivämääriä käsitellään. Vaihtoehtoja sisäänrakennetulle Date-moduulille löytyy yhteisön tekemistä paketeista, kuten elm-time ja justinmimbs/date, jotka tarjoavat lisätoiminnallisuuksia.

## Katso Myös
- Elm Date moduulin dokumentaatio: https://package.elm-lang.org/packages/elm-lang/core/latest/Date
- Elm Time, laajennettu päivämäärä ja aika-kirjasto: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- "Elm ja Päivämäärät": opas yhteisön luomille kirjastoille ja niiden käytölle.
