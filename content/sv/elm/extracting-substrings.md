---
title:    "Elm: Extraherande av substrängar"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrings är en användbar teknik inom programmering som låter dig isolera och manipulera delar av en större textsträng. Det är speciellt viktigt inom funktionell programmering, som Elm, där du vill undvika att mutera variabler.

## Hur man gör det

Extrahering av substrings i Elm är enkelt och kan göras med hjälp av funktionen `String.slice`. För att extrahera en del av en sträng, behöver du bara ge den start- och slutpositionen för delen du vill isolera. Nedanför finns ett exempel på hur du kan använda `String.slice` för att extrahera en understräng från en större sträng.

```
Elm String.slice Exempel

sträng = "Den här texten har många ord."

startposition = 4
slutposition = 8

String.slice startposition slutposition sträng
```

Output: `här `

Som du ser i exemplet, tar `String.slice` tre argument: startposition, slutposition och strängen du vill extrahera delen från.

## Djupdykning

Förutom att extrahera en del av en sträng, kan du också använda `String.slice` för att byta ut en del av strängen med en annan text. Det gör du genom att bara ge en ny sträng som det tredje argumentet till `String.slice`.

Du kan också använda `String.slice` för att hitta och ersätta ett specifikt mönster i en sträng. Genom att kombinera `String.slice` med andra funktioner som `String.indexOf` kan du manipulera textsträngar på olika sätt.

## Se också

- Dokumentation för `String.slice` i Elm: https://package.elm-lang.org/packages/elm-lang/core/latest/String#slice
- En guide till vanliga strängoperationer i Elm: https://elmprogramming.com/string-manipulation-elm.html
- Officiell webbsida för Elm: https://elm-lang.org/