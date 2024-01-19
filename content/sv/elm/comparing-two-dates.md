---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Jämföra Datum i Elm Programmering

## Vad & Varför?
Jämföra två datum innebär kontrollera vilket datum som kommer först eller om de två datumen är desamma. Programmerare gör detta för att hantera tidslinjer, schemalägga uppgifter och utföra tidsspecifika beräkningar.

## Hur man gör:
Här är ett enkelt exempel för att jämföra två datum i Elm:

```Elm
module Main exposing (..)
import Date exposing (Date, year, month, day, fromCalendarDate)

dato1 = fromCalendarDate 2021 1 1
dato2 = fromCalendarDate 2022 1 1

main =
    if dato1 < dato2 then
      "Dato1 är före Dato2"
    else if dato1 == dato2 then
      "Dato1 är samma som Dato2"
    else
      "Dato1 är efter Dato2"
```

Om du kör den här koden kommer utdatan att vara "Dato1 är före Dato2", eftersom 1 januari 2021 kommer före 1 januari 2022.

## Djupdykning
Historiskt sett har jämförelse av datum alltid varit a central del av programmering. Innan Elm och moderna programmeringsspråk utvecklades, var denna process mycket mer komplicerad och krävde omfattande användning av matematik och konverteringar.

Ett alternativ till direkta jämförelser är att konvertera båda datumen till Unix-tid (sekunder sedan den 1 januari 1970), och sedan jämföra dessa värden. Detta kan dock vara mindre läsbart och kan ge överraskande resultat om tidzoner inte hanteras korrekt.

Under huven, när du skriver `dato1 < dato2`, använder Elm operatorer definierade för `Date`-datatypen. Dessa operatorer utför försiktigt en jämförelse av åren, månaderna och sedan dagarna, vilket ger ett exakt resultat.

## Se Även
För ytterligare detaljer och användningssituationer, se dokumentation för Elm Date: https://package.elm-lang.org/packages/elm/time/latest/