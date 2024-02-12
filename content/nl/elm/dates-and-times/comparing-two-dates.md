---
title:                "Twee datums vergelijken"
aliases:
- /nl/elm/comparing-two-dates/
date:                  2024-01-28T21:56:22.275534-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het vergelijken van twee datums betekent uitzoeken welke van de twee eerder is of hoeveel tijd er tussen hen in zit. Programmeurs doen dit om zaken zoals deadlines, schema's of tijdgebonden functies te beheren.

## Hoe:

Elm maakt datumvergelijkingen eenvoudig. Laten we zeggen dat je twee datums hebt. Hier is hoe je zou controleren welke als eerste komt:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates datum1 datum2 =
    if datum1 < datum2 then
        LT  -- datum1 is eerder dan datum2
    else if datum1 > datum2 then
        GT  -- datum1 is later dan datum2
    else
        EQ  -- datums zijn hetzelfde

-- Voorbeeldgebruik:
let
    datum1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- Voeg je eerste datum toe in POSIX-tijd
    datum2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- En je tweede datum in POSIX-tijd
in
compareDates datum1 datum2
-- Uitvoer zal LT, GT, of EQ zijn
```

Je kunt ook het verschil in milliseconden berekenen:

```Elm
tijdsverschil : Posix -> Posix -> Time.Duration
tijdsverschil datum1 datum2 =
    Time.millisToPosix datum1 - Time.millisToPosix datum2

-- Voorbeeldgebruik:
let
    datum1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    datum2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
tijdsverschil datum1 datum2
-- Uitvoer: Duur in milliseconden
```

## Diepe Duik
Elm slaat datums op als `Posix`, wat milliseconden sinds het Unix-tijdperk (1 januari 1970, UTC) voorstelt. Dit is een veelvoorkomende benadering, die zijn wortels deelt met Unix-tijd, en het vergemakkelijkt datummanipulatie en -opslag.

Hoewel Elm's kernbibliotheek basisdatumbewerkingen biedt, bestaan er alternatieven zoals `justinmimbs/date` voor complexere operaties.

Bij het implementeren van datumvergelijkingen, onthoud dat tijdzones dingen kunnen compliceren. Elm's `Time` module gaat uit van UTC, wat betekent dat je gespaard blijft van problemen met zomertijd, maar je moet mogelijk aanpassingen doen voor lokale tijdzones in je applicatie.

## Zie Ook
- Elm Time module: https://package.elm-lang.org/packages/elm/time/latest/
- Justin Mimbs' Date pakket voor Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Unix-tijd: https://nl.wikipedia.org/wiki/Unix-tijd
