---
title:                "Elm: Kapitalisera en sträng"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att förstå hur man använder Elm för att konvertera text till stora bokstäver kan vara användbart för att skapa en mer strukturerad och enhetlig utseende på ett projekt. Det kan också underlätta vid filtrering och sökning av textsträngar.

## Hur man gör

För att konvertera en textsträng till stora bokstäver i Elm, kan man använda funktionen `String.toUpper`. Här är ett exempel på hur man skulle kunna använda den:
 
```Elm
String.toUpper "detta är en textsträng" 

-- Output: "DETTA ÄR EN TEXTSTRÄNG"
```

Om man vill konvertera endast den första bokstaven i en textsträng till stor bokstav, kan man använda funktionen `String.toCapitalized` istället.

```Elm
String.toCapitalized "detta är en textsträng" 

-- Output: "Detta är en textsträng"
```

## Djupdykning

Det finns flera sätt att konvertera textsträngar till stora bokstäver i Elm, inklusive användning av regelbundna uttryck och användning av inbyggda funktioner som `String.toUpperFirst` och `String.toTitle`.

En vanlig teknik är att först konvertera textsträngen till små bokstäver med hjälp av funktionen `String.toLower`, och sedan konvertera den till stora bokstäver med hjälp av `String.toUpper`.

## Se även

- [Elm Documentation: String Module](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Tutorial: String Module](https://guide.elm-lang.org/strings/)
- [Learn You An Elm: Strings](https://learnyouanelm.com/basics/strings)