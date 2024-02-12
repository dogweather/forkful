---
title:                "Een datum uit een string parsen"
date:                  2024-01-28T22:03:54.300166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het parseren van een datum uit een tekenreeks betekent het converteren van tekst die een datum voorstelt naar een formaat waarmee een programma kan werken. Programmeurs doen dit om datums te manipuleren—denk aan het sorteren van evenementen of het berekenen van tijdsduren—in apps die gaan over planning, deadlines, en meer.

## Hoe te:
Elm gebruikt de `Date` module om met datums om te gaan, maar voor zover mijn kennis reikt in begin 2023, is er geen ingebouwde Elm-bibliotheek voor het parseren van datums uit tekenreeksen. Je zult waarschijnlijk een pakket zoals `justinmimbs/date` gebruiken om de taak uit te voeren. Zo ga je ermee om:

```Elm
import Date
import Date.Extra.Parse as DateParse

-- Een datum uit een tekenreeks parseren
parseDate : String -> Maybe Date
parseDate dateString =
    DateParse.fromIsoString dateString

-- Voorbeeld van gebruik
main =
    case parseDate "2023-04-01" of
        Just date ->
            -- Succesvol geparseerd, doe iets met `date`
            ...

        Nothing ->
            -- Parsen mislukt, fout afhandelen
            ...
```
Voorbeelduitvoer voor het parseren van `"2023-04-01"` zou een `Date` waarde zijn, en bij mislukking krijg je `Nothing`.

## Diepere Duik
In de vroege dagen werden JavaScript's Date methoden vaak direct gebruikt in Elm via poorten, maar dit was niet ideaal. Dingen verbeterden met pakketten zoals `justinmimbs/date`, die meer Elm-achtige manieren bieden om met datums om te gaan. Elm's sterke typesysteem en nadruk op betrouwbaarheid geven de voorkeur aan expliciete parsemethoden, waar mislukking duidelijk wordt aangegeven door `Maybe` types, in tegenstelling tot JavaScript's soms onvoorspelbare datum parsing.

Vanaf de huidige versie zijn er geen ingebouwde string-naar-datum functies in Elm's kern `Date` module, wat verklaart waarom gemeenschapspakketten zo belangrijk zijn. Alternatieven zoals `ryannhg/date-format` kunnen datums formatteren naar tekenreeksen, maar parseren is een ander beest—daarom is `justinmimbs/date` meer geschikt voor deze taak.

Met betrekking tot implementatie, houdt Elm's aanpak je app robuust: ongeldige datums zullen het niet onverwacht laten crashen, dankzij de duidelijke `Maybe Date` signaalgeving of het parsen geslaagd is.

## Zie Ook
- Elm Date documentatie: https://package.elm-lang.org/packages/elm/time/latest/
- justinmimbs/date bibliotheek voor het parseren: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- elm-community/elm-time voor extra tijd hulpmiddelen (indien nodig): https://package.elm-lang.org/packages/elm-community/elm-time/latest/
