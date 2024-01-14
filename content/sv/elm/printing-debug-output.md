---
title:                "Elm: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att skriva ut debug meddelanden är en viktig del av programmeringsprocessen, eftersom det ger feedback på hur koden körs och hjälper till att hitta eventuella fel. Det är också ett bra sätt att förstå varför en viss del av koden fungerar eller inte fungerar som förväntat.

## Hur man gör det
För att skriva ut debug meddelanden i Elm, kan man använda funktionen `Debug.log` som tar två argument: en sträng som beskriver meddelandet och ett värde som ska skrivas ut. Här är ett exempel på hur man kan använda funktionen:

```Elm
import Debug exposing (log)

myNumber = 5
myString = "Hej!"

log "Detta är en siffra" myNumber
log "Detta är en sträng" myString
```

Detta kommer att skriva ut följande i konsolen:

```
Detta är en siffra: 5
Detta är en sträng: Hej!
```

Man kan också använda funktionen `Debug.toString` för att konvertera ett värde till en sträng, vilket är användbart om man vill skriva ut komplexa datatyper som listor eller tupler. Det är också möjligt att skriva ut flera värden samtidigt genom att använda en kommeraseparerad lista av värden som argument till `Debug.log`.

## Djupdykning
När man använder debug meddelanden är det viktigt att notera att de bara skrivs ut i utvecklingsläge och inte i produktionsläge. Det är också viktigt att bara använda `Debug.log` för att få information om koden och inte som en del av den faktiska koden, eftersom funktionen kommer att köras även om koden aldrig används. Detta kan leda till onödig prestandaförlust.

## Se även
- Officiell Elm dokumentation för `Debug` modulen: https://package.elm-lang.org/packages/elm/core/1.0.0/Debug
- Elm repl för att testa och experimentera med debug meddelanden: https://elm-lang.org/try