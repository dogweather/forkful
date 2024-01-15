---
title:                "Skriva till standardfel"
html_title:           "Elm: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför 

Att skriva till standard error är ett vanligt verktyg för utvecklare som vill felsöka och ta reda på vad som går fel i deras kod. Det är ett enkelt sätt att få ut information om eventuella fel och var de uppstod, vilket underlättar förbättringar och reparationer av koden.

## Så här gör du 

För att använda standard error i Elm, behöver du först importera modulen `Platform` och `Debug`. Sedan kan du enkelt skriva till standard error med hjälp av funktionen `Debug.crash`, som tar en sträng som argument. Här är ett exempel:

```elm
import Platform
import Debug

main =
    Debug.crash "This is an error message"
```

Detta kommer att skriva ut "This is an error message" till standard error. Du kan också skicka in andra typer av värden till `Debug.crash`, som t.ex. en lista eller en tupel.

## Djupdykning 

Standard error är en del av Elm:s "Debug" modul, som ger utvecklare möjlighet att felsöka och logga information om sin kod. Det är särskilt användbart för att fånga och hantera fel som uppstår under körning av ett program.

Förutom att skriva till standard error, kan du också använda `Debug.log` för att logga information till standard out. `Debug.log` tar två argument, en sträng för att beskriva vad som loggas och själva värdet som ska loggas. Den här funktionen är användbar för att spåra värden och kontrollera att de är korrekta under körning.

Det finns också flera andra funktioner i Elm:s "Debug" modul som kan hjälpa till med felsökning. Till exempel `Debug.todo`, som används för att signalera att en viss del av koden ännu inte är implementerad och behöver färdigställas.

## Se även 

- Elm:s officiella dokumentation om "Debug" modulen: [https://guide.elm-lang.org/debugging/](https://guide.elm-lang.org/debugging/)
- En guide för felsökning i Elm: [https://www.twilio.com/blog/2018/07/debugging-elm-apps.html](https://www.twilio.com/blog/2018/07/debugging-elm-apps.html)