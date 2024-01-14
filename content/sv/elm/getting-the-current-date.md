---
title:    "Elm: Hämta aktuellt datum"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumen kan vara en viktig del av många Elm-program. Det kan användas för att visa aktuell tidsstämpel, sätta deadlines eller kontrollera behörigheter.

## Så här gör du

För att få den nuvarande datumen i Elm, behöver vi använda paketet "time". För att installera paketet, skriv följande i din terminal:

```Elm
elm install time
```

Nästa steg är att importera paketet i din Elm-fil:

```Elm
import Time
```

Nu kan vi använda "Time.now" funktionen för att få den nuvarande datumen som en "Posix" tidsstämpel:

```Elm
currentDate = Time.now
```

För att konvertera tidsstämpeln till ett mer läsbart format, kan vi använda "Time.toMillis" funktionen:

```Elm
formattedDate = Time.toMillis currentDate
```

Detta kommer att ge oss den nuvarande datumen som en "Int" i millisekunder. Om du vill ha en mer specifik format, kan vi använda "Time.format" funktionen. Till exempel, om vi vill ha datumen i formatet ÅR-MÅNAD-DAG:

```Elm
formattedDate = Time.format "%Y-%m-%d" currentDate
```

Slutligen, om du vill ha en enklare strängversion av datumen, kan du använda "Time.Format.toHumanString" funktionen:

```Elm
formattedDate = Time.Format.toHumanString currentDate
```

## Djupdykning

Paketen "Time" innehåller flera användbara funktioner för att hantera datum och tid i Elm. En viktig sak att notera är att alla funktioner arbetar med "Posix" tidsstämplar, vilket är antalet sekunder sedan 1 januari 1970. Detta möjliggör enkel omvandling till och från andra tidsformat och gör det enkelt att utföra beräkningar på datumen.

En annan intressant funktion är "Time.millisToPosix", som gör omvandling från millisekunder till "Posix" tidsstämpel. Detta är användbart om du har datumen i en annan form och vill få dem i "Posix" format.

## Se även

- Officiell dokumentation för Time-paketet i Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Ett exemplariskt projekt som använder Time-paketet för att visa den nuvarande tidsstämpeln: https://github.com/elm/time-example