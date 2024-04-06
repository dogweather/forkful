---
date: 2024-01-20 17:34:44.990911-07:00
description: 'How to: - Kuinka: Sample output.'
lastmod: '2024-04-05T22:38:57.083253-06:00'
model: gpt-4-1106-preview
summary: '- Kuinka: Sample output.'
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## How to: - Kuinka:
```Elm
module ConcatExample exposing (..)

import Html exposing (text)

-- Yhdista kaksi merkkijonoa
concatStrings : String -> String -> String
concatStrings a b =
    a ++ b

-- Kayttö
main =
    text (concatStrings "Hei, " "maailma!")
```

Sample output:
```
"Hei, maailma!"
```

## Deep Dive - Syväsukellus
Elm:ssä merkkijonojen yhdistäminen tapahtuu `++` operaattorilla. Historiallisesti eri kielessä on eri tapoja yhdistää merkkijonoja, kuten JavaScriptissä `+` operaattori tai PHP:ssä `.` operaattori. Elm pitää yksinkertaisuudesta ja selkeydestä, siksi käyttää `++` operaattoria.

Merkkijonojen yhdistäminen on yksinkertainen, mutta voit törmätä suorituskykyongelmiin suurilla datamäärillä. Tätä varten Elm optimoi yhdistämisen sisäisesti käyttäen tehokkaita algoritmeja, kuten köysialgoritmi (rope algorithm), mikä auttaa suorituskyvyssä.

Vaihtoehtoina suorille merkkijonoyhdistelmille ovat template stringit tai merkkijonoliteraalit (jotka eivät ole Elm:ssä käytössä), tai merkkijonolistojen kokoaminen, joka voi olla tehokkaampi isoilla merkkijonoilla. Elm:ssä voit myös käyttää `String.join` funktiota yhdistämään merkkijonojen listoja.

## See Also - Katso Myös
- Elm:n virallinen dokumentaatio merkkijonoista: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm kielen optimointikeskustelu: https://elm-lang.org/news/small-assets-without-the-headache
- Rope-algoritmi, Wikipedia: https://en.wikipedia.org/wiki/Rope_(data_structure)
