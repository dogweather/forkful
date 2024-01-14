---
title:                "Haskell: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions, eller reguljära uttryck, är ett kraftfullt verktyg som används för att söka efter och manipulera textsträngar. Genom att lära sig att använda reguljära uttryck, kan du effektivisera dina program och göra textbehandling enklare och snabbare.

## Hur man gör

För att använda reguljära uttryck i Haskell, behöver du importera "Text.Regex" modulen. Sedan kan du använda funktioner som "regexMatch" för att söka efter en matchning i en textsträng. Här är ett exempel:

```Haskell
import Text.Regex

-- Skapar ett reguljärt uttryck för att matcha ett telefonnummer
telefonnummer <- regexFromText "^[0-9]{3}-[0-9]{3}-[0-9]{4}$"

-- Söker efter matchning i en textsträng
regexMatch telefonnummer "Telefonnummer: 123-456-7890"
-- Output: Just "123-456-7890"
```

Som du kan se i exemplet ovan, kan du använda reguljära uttryck för att söka efter specifika mönster i en textsträng, som i det här fallet ett telefonnummer. Du kan också använda andra funktioner som "regexReplace" för att byta ut en del av en textsträng som matchar med ditt reguljära uttryck.

## Djupdykning

Det finns många olika sätt att använda reguljära uttryck på i Haskell, och det kan ta lite tid att lära sig alla funktioner och olika metoder för att manipulera textsträngar. Men regelbundna uttryck är en viktig del av Haskell-programmering och kan hjälpa dig att göra dina program mer effektiva och pålitliga.

En av de största fördelarna med att använda reguljära uttryck är att de är plattformsoberoende och kan användas på olika operativsystem och språk. Dessutom finns det många online-resurser och bibliotek som kan hjälpa dig att lära dig mer om reguljära uttryck och deras användning i Haskell.

## Se även

- [Haskell.org Regular Expressions](https://www.haskell.org/onlinereport/regex.html)
- [Real World Haskell: Regular expressions](https://www.realworldhaskell.org/bonus-parsing-with-parsec.html#regular-expressions)
- [RegExr: Learn, Build, & Test Regular Expressions](https://regexr.com/)