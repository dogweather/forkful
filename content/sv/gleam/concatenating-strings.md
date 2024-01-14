---
title:                "Gleam: Sammanslående strängar"
simple_title:         "Sammanslående strängar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför?

Att sammanfoga strängar är en viktig del av programmering. Det möjliggör för dig att skapa mer dynamiska och anpassade meddelanden, texter eller data. Det kan vara särskilt användbart när du behöver konstruera meddelanden som innehåller variabler eller data från olika källor.

## Hur gör man?

För att sätta ihop olika strängar kan du använda dig av den inbyggda funktionen `<>` i Gleam. Detta gör att du kan lägga ihop två eller flera strängar till en enda lång sträng. Här är ett enkelt exempel på hur det skulle kunna se ut i Gleam:

```Gleam
let namn = "Kalle"
let text = "Hej, mitt namn är " <> namn
```
Output: `Hej, mitt namn är Kalle`

Det är också möjligt att använda `%s` för att lägga in variabler eller värden i en sträng. Till exempel:

```Gleam
let namn = "Kalle"
let alder = 25
let text = "Hej, mitt namn är %s och jag är %d år gammal." <> (namn, alder)
```
Output: `Hej, mitt namn är Kalle och jag är 25 år gammal.`

## Djupdykning

När du sätter ihop strängar är det viktigt att tänka på att det kan finnas vissa begränsningar. Till exempel kan du bara sätta ihop strängar av samma typ, till exempel "string" till "string". Det är också möjligt att sätta ihop flera strängar samtidigt, till exempel `sträng1 <> sträng2 <> sträng3`. Detta gör att du kan bygga ännu mer komplexa strängar efter behov.

Det finns också andra inbyggda funktioner i Gleam som kan hjälpa dig att bygga strängar, som `text.concat()`. Det är alltid bra att utforska och experimentera med olika metoder för att hitta den som passar bäst för dina behov.

## Se även

- [Officiell Gleam dokumentation om strängar](https://gleam.run/book/tour/types_and_operations.html#strings)
- [En bra guide på engelska om hur man sätter ihop strängar i Gleam](https://dev.to/mlos Designs/merge-strings-in-gleam-g48)

Tack för att du läste och lycka till med dina strängkonkaterneringsäventyr!