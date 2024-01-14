---
title:    "Elm: Användning av reguljära uttryck"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck är ett kraftfullt verktyg inom programmering, som låter dig söka och manipulera text på ett flexibelt sätt. Genom att använda reguljära uttryck kan du effektivt söka igenom texter, ersätta specifika ord eller utföra andra komplicerade uppgifter. Det är ett viktigt verktyg som alla Elm-programmerare bör känna till.

## Hur man använder reguljära uttryck i Elm

För att använda reguljära uttryck i Elm behöver du importera modulen `Regex` och därefter skapa ett uttryck med hjälp av funktionen `Regex.fromString` som tar in en sträng som beskriver det mönster du vill matcha. Sedan kan du använda ditt skapade uttryck med hjälp av funktioner som `Regex.find` och `Regex.replace`, och utföra olika operationer på texten baserat på det angivna mönstret.

Här är ett exempel på hur du kan använda reguljära uttryck för att hitta alla förekomster av siffror i en sträng:

```Elm
import Regex exposing (..)

text = "Jag är 25 år gammal"

uttryck = Regex.fromString "\\d+"

Regex.find uttryck text
--> Just [ { match = "25", index = 8, number = 0 } ]
```

Som du kan se kommer funktionen `Regex.find` att returnera en lista med alla matchningar som följer det angivna mönstret, och var i strängen dessa matchningar förekommer. Du kan sedan använda dessa matchningar för att ersätta eller manipulera texten på olika sätt.

## Djupdykning i reguljära uttryck

Reguljära uttryck kan vara komplicerade och ta lite tid att lära sig, men de kan vara väldigt användbara när du väl behärskar dem. Det finns många olika uttryck du kan använda för att matcha specifika mönster, till exempel:

- `.*` för att matcha alla tecken
- `\\d` för att matcha siffror
- `\\w` för att matcha bokstäver, siffror och understreck
- `\\s` för att matcha blanksteg
- `[]` för att matcha en grupp av tecken
- `+` för att matcha en eller flera förekomster av det angivna mönstret

Det finns också möjlighet att använda uttryck för att fånga vissa delar av texten som matchas, vilket kan vara användbart om du vill använda denna information för att utföra en viss operation på texten.

För att lära dig mer om reguljära uttryck kan du kolla på Elm:s dokumentation för modulen `Regex` eller söka efter tutorials och exempel online.

## Se också

- [Elm:s officiella dokumentation för modulen Regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Reguljära uttryck för nybörjare](https://en.wikipedia.org/wiki/Regular_expression#Basic_concepts)
- [Reguljära uttryck i praktiken](https://www.regular-expressions.info/)