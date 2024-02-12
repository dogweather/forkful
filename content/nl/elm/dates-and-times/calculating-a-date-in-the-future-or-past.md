---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases:
- nl/elm/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:17.699083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een toekomstige of verleden datum berekenen is gewoon een gegeven datum aanpassen met een bepaalde hoeveelheid tijd. Programmeurs doen dit om deadlines, evenementen, herinneringen—alles wat met data te maken heeft—te hanteren.

## Hoe:
De `Time` module van Elm en het `justinmimbs/time-extra` pakket laten ons gemakkelijk met datums knoeien.

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: aantal dagen om toe te voegen (negatief om af te trekken)
-- @fromDate: startdatum in Posix-formaat

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- Gebruik
-- Vergeet niet, Elm telt tijd in milliseconden sinds het Unix-tijdperk.

sampleDate = Time.millisToPosix 1580515200000  -- 1 Februari 2020 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- Voegt 10 dagen toe
pastDate = calculateDate -15 sampleDate        -- Trekt 15 dagen af

-- voorbeelduitvoer:
-- futureDate -> 1581552000000  -- 12 Februari 2020 00:00:00 UTC
-- pastDate -> 1580006400000    -- 17 Januari 2020 00:00:00 UTC
```

## Diepgaand
Vroeger was het omgaan met datums in programmeren een pijn. Verschillende systemen, formaten en tijdzones gaven iedereen hoofdpijn. Elm's 'Time' module, gebaseerd op het Unix-tijdsysteem (milliseconden sinds 1970), standaardiseert dit. Het pakket `justinmimbs/time-extra` vereenvoudigt verder het uitvoeren van operaties op datums, zoals het toevoegen of aftrekken van dagen.

Alternatieven? Andere talen hebben hun eigen bibliotheken, zoals Python's `datetime` of JavaScript's `Date`. Maar Elm's aanpak biedt sterke typen en zuiverheid, waardoor bugs worden verminderd.

Naast het toevoegen van dagen, kunt u ook werken met maanden, jaren, of zelfs uren en minuten. De functies in Elm en in pakketten zoals `time-extra` focussen op onveranderlijkheid en zuivere functies—dit betekent geen bijeffecten. Wanneer je een nieuwe datum berekent, blijft de oorspronkelijke ongewijzigd.

## Zie Ook
- Elm `Time` module: https://package.elm-lang.org/packages/elm/time/latest/
- `justinmimbs/time-extra` pakket: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Elm Gids over Tijd: https://guide.elm-lang.org/effects/time.html
