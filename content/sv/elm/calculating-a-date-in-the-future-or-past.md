---
title:                "Elm: Beräkning av ett datum i framtiden eller i det förflutna"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna datum i framtiden eller förflutna är en nyttig färdighet för att hantera tidsbaserade data och uppdrag. Med hjälp av Elm, kan du enkelt utföra dessa beräkningar och optimera din programmering.

## Så här gör du
För att beräkna ett datum i framtiden eller förflutna i Elm, kan du använda DateTime-modulen. För att beräkna ett datum i framtiden, använd funktionen `add` med antalet dagar, veckor eller månader du vill lägga till. Se exemplet nedan:

```elm
import DateTime exposing (add, Days)

futureDate = add 10 Days Today
```

I detta exempel lägger vi till 10 dagar till dagens datum och lagrar det nya datumet i en variabel som heter `futureDate`. Du kan också lägga till andra tidsenheter som veckor eller månader genom att byta ut `Days` med motsvarande enhet.

Om du vill beräkna ett datum i förflutna, kan du använda funktionen `subtract` istället. Se exempel nedan:

```elm
import DateTime exposing (subtract, Days)

pastDate = subtract 5 Days Today
```

I detta exempel subtraherar vi 5 dagar från dagens datum och lagrar det nya datumet i en variabel som heter `pastDate`.

## Djupdykning
Du kan också använda funktionerna `add` och `subtract` med ett specifikt datum istället för dagens datum. Om du till exempel vill beräkna ett datum i förflutna med utgångspunkt från 1 januari 2021, kan du göra det genom att använda `add` eller `subtract` med detta datum som utgångspunkt.

För att göra detta, måste du först omvandla detta datum till en följd av millisekunder genom att använda funktionen `toMillis` från DateTime-modulen. Se exemplet nedan:

```elm
import DateTime exposing (toMillis, add)

januaryFirst = January 1 2021

milliseconds = toMillis januaryFirst
```

Vi omvandlar här 1 januari 2021 till millisekunder och sparar resultatet i en variabel som heter `milliseconds`. Sedan kan vi använda denna variabel tillsammans med `add` eller `subtract` för att beräkna datum i förflutna från detta specifika datum.

## Se även
- Elm officiell dokumentation för DateTime-modulen: https://package.elm-lang.org/packages/elm/core/latest/DateTime