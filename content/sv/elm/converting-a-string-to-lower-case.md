---
title:                "Elm: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med Elm, kan det ibland vara nödvändigt att konvertera en sträng till små bokstäver, oavsett om det är för att jämföra strängar eller för att visa dem i en enhetlig stil. Detta kan göras på ett enkelt och effektivt sätt med hjälp av inbyggda funktioner i Elm.

## Hur man gör det

För att konvertera en sträng till små bokstäver i Elm, kan du använda funktionen `String.toLower`. Se följande exempel:

```Elm
stäng = "ELM PROGRAMMERING"
konverteradSträng = String.toLower stäng
```

Koden ovan kommer att konvertera `stäng` till den nya variabeln `konverteradSträng`, som kommer att ha värdet "elm programmering". Det är viktigt att notera att `String.toLower` funktionen returnerar en helt ny sträng, så se till att tilldela den till en ny variabel.

## En djupdykning

Det finns flera andra inbyggda funktioner i Elm som kan komma till nytta när du arbetar med strängar och deras konvertering till små bokstäver. Till exempel `String.toUpper` som gör motsatsen, `String.toTitle` som konverterar till titel-case och `String.reverse` som vänder på ordningen av bokstäver i en sträng. Du kan också använda `String.foldl` för att göra mer avancerade manipulationer på en sträng.

## Se även

- [Officiell dokumentation för strängar i Elm](https://elm-lang.org/docs/strings)
- [En tutorial för Elm på svenska](https://www.elm-tutorial.org/sv/)