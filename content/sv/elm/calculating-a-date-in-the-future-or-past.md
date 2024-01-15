---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Elm: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller förflutna är en användbar funktion för många programmerare. Det kan hjälpa till att hantera schemaläggning eller för att skapa en dynamisk applikation som visar relevanta datum för användaren.

## Så här gör du

För att beräkna ett datum i framtiden eller förflutna i Elm, behöver du använda funktionen `Date` från Elm's inbyggda tidsmodul. Det finns två parametrar som måste anges för att få fram ett korrekt datum:

1. Det aktuella datumet, representerat av en `Date`-typ.
2. Antalet dagar som ska läggas till eller subtraheras från det aktuella datumet, representerat av ett heltal.

Här är ett exempel på hur du skulle använda funktionen för att få dagens datum plus fem dagar framåt:

```Elm
import Date exposing (..)

today = Date.fromDate 2021 5 24

futureDate = Date.addDays 5 today

-- futureDate blir nu Date 2021 5 29
```
Det är också möjligt att använda en negativ siffra för att få ett datum i förflutna. Här är ett exempel på hur du skulle få datumet tre dagar bakåt i tiden:

```Elm
import Date exposing (..)

today = Date.fromDate 2021 5 24

pastDate = Date.addDays -3 today

-- pastDate blir nu Date 2021 5 21
```

Du kan också använda funktionen `addWeeks`, `addMonths`, `addYears` för att få ett datum med en annan tidsperiod än dagar.

## Djupdykning

När du använder funktionerna för att beräkna ett datum i framtiden eller förflutna, är det viktigt att vara medveten om några saker. Först och främst måste du se till att det aktuella datumet är korrekt formaterat enligt `yyyy mm dd`-formatet. Om detta inte stämmer kan funktionen ge ett felaktigt datum.

För det andra, om du försöker skapa ett datum som inte existerar, till exempel den 31 februari, kommer funktionen att anpassa det till ett giltigt datum såsom den 2 mars. Detta kan orsaka problem om du har specifika datumkrav i din applikation.

Slutligen, om du behöver hantera tidszoner, måste du använda en extern tidsmodul från en tredjepartsleverantör, eftersom Elm's inbyggda tidsmodul inte har stöd för det.

## Se även

- [Officiell tidsmodul för Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Exempel på att använda funktionen `Date` i en Elm-applikation](https://gist.github.com/itsmewulf/7edac8fe31db9278452ffb3a0190f35c)