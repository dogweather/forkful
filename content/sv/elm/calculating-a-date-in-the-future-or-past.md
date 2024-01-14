---
title:    "Elm: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

I detta inlägg kommer vi att prata om hur man kan beräkna datum i framtiden eller i det förflutna i Elm-programmeringsspråket. Att kunna utföra dessa beräkningar kan vara till nytta för många projekt, från enkel kalenderintegration till mer avancerade tidsbaserade funktioner.

## Hur man gör det

Först och främst måste vi importera paketet "elm/time" i vår Elm-fil. Detta ger oss tillgång till olika tidsfunktioner som vi kommer att använda.

```elm
import Time exposing (..)
```

För att beräkna ett datum i framtiden eller förflutnanen måste vi först ha ett "Time" värde att utgå ifrån. Detta värde representerar antalet millisekunder som har passerat sedan 1 januari 1970. Vi kan använda funktionen "millisToPosix" för att konvertera ett datum till detta värde.

```elm
let dateInEpoch = millisToPosix 1609459200000
```

För att beräkna ett datum i framtiden, lägg bara till ett antal millisekunder till detta värde.

```elm
let futureDate = add dateInEpoch (days 10) --> 1600560000000
```

För att beräkna ett datum i det förflutna, subtrahera ett antal millisekunder från detta värde.

```elm
let pastDate = sub dateInEpoch (hours 48) --> 1608672000000
```

Det är också möjligt att konvertera tillbaka detta värde till ett vanligt datumformat med hjälp av funktionen "posixToMillis".

```elm
let formatedDate = posixToMillis futureDate
```

## Djupdykning

Om du vill beräkna mer komplexa datum kan du använda funktionen "since" som tar in två tidsvärden och returnerar antalet millisekunder som har passerat mellan dem.

```elm
let millisecondsPassed = since futureDate pastDate --> 10000000
```

Det är också möjligt att använda funktionen "inTenthOfMilliseconds" för att få antalet tiondels millisekunder mellan två datum.

```elm
let tenthMillisecondsPassed = inTenthOfMilliseconds futureDate pastDate --> 100000
```

## Se även

- Elm dokumentation om tidsoperationer: <https://package.elm-lang.org/packages/elm/time/latest>
- Så här beräknar du mellan tider i JavaScript: <https://www.w3schools.com/jsref/jsref_time.asp>