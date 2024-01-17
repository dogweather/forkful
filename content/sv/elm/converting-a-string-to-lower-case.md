---
title:                "Omvandla en sträng till gemener"
html_title:           "Elm: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att omvandla en sträng till små bokstäver innebär att alla bokstäver i en sträng konverteras till deras motsvarande små bokstäver. Det är en vanlig funktion som används av programmerare för att standardisera strängar och göra det enklare att söka och jämföra strängar.

## Hur:
```Elm
import String
String.toLower "ELM IS AWESOME!" -- Output: "elm is awesome!"
```

## Djupdykning:
Historiskt sett har konvertering av strängar till små bokstäver använts för att lösa problem med att söka och jämföra strängar som inte höll sig till en konsekvent skiftlägeskonvention. Det finns alternativa sätt att konvertera strängar till små bokstäver, såsom att använda inbyggda funktioner i språket eller använda regex-uttryck. I Elm används funktionen `String.toLower` för att konvertera strängar till små bokstäver. Detta sker genom att använda Unicode-normaliseringsalgoritmen, vilket gör att denna funktion fungerar lika bra för internationella tecken som för enkel byte-tecken.

## Se också:
- Elm's officiella dokumentation för [String-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-styleguide](https://github.com/NoRedInk/style-guide/blob/master/elm.md) för rekommendationer om att använda `String.toLower` för att hantera strängar i en konsekvent form