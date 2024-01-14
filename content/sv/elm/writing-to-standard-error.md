---
title:                "Elm: Skriva till standardfel"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel är en viktig del av Elm-programmering eftersom det gör det möjligt att fånga och hantera felmeddelanden i våra program. Detta gör det möjligt för oss att förbättra kvaliteten på våra program och ge användarna en bättre upplevelse.

## Hur man gör
För att skriva till standardfel i Elm använder vi funktionen `Debug.log`, som tar två argument: en sträng som beskriver vad vi vill logga och värdet vi vill logga. Här är ett enkelt exempel på hur man använder `Debug.log` för att skriva till standardfel:

```Elm
Debug.log "Hej" "Världen"
```

Om vi kör detta program kommer vi att få följande utdata i vårt terminalfönster:

```
Hej = Världen
```

Detta visar att vi har loggat värdet "Världen" till standardfel och att det är kopplat till etiketten "Hej". Genom att använda olika strängar kan vi logga olika delar av vårt program för att få en bättre förståelse för hur det fungerar.

## Djupdykning
En viktig sak att komma ihåg när man använder `Debug.log` är att det bara bör användas för utvecklingsändamål och inte i produktionskoden. Detta beror på att det kan ha en negativ inverkan på prestandan för våra program. Det är också viktigt att se till att alla `Debug.log`-anrop är borttagna eller inaktiverade innan vi publicerar vårt program för användning.

Vi kan också använda `Debug.todo` för att skriva till standardfel när vi har kod som inte är färdig eller behöver förbättras. Detta är ett bra sätt att påminna oss själva om att återvända till en viss del av koden senare och göra förbättringar.

## Se också
- [Debug module i Elm documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Writing to standard error in Elm on ThoughtBot](https://thoughtbot.com/blog/writing-to-standard-error-in-elm)