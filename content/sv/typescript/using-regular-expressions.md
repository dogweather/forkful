---
title:                "Användning av reguljära uttryck"
html_title:           "TypeScript: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Reguljära uttryck är en metod som används för att söka efter tillhörigheter av textmönster inom en sträng. Detta är användbart för programmerare som behöver effektivt hitta, ersätta eller manipulera specifika delar av texter i sina kodprojekt.

## Så här:
Att använda reguljära uttryck är enkelt med TypeScript. Här är ett exempel på hur man kan hitta alla "a" i en sträng och ersätta dem med "b":
```TypeScript
let str = "abcde";
let modifiedStr = str.replace(/a/g, "b");
console.log(modifiedStr); // bcdeb
```

## Djupdykning:
Reguljära uttryck har funnits sedan 50-talet och har utvecklats till en standard för textmönster-sökning. Alternativ till reguljära uttryck inkluderar metoder som strängmanipulering och bibliotek som lodash, men dessa är oftast mer komplicerade och inte lika effektiva. Implementeringen av reguljära uttryck skiljer sig åt beroende på programmeringsspråk men i TypeScript behöver man bara använda RegExp-klassen för att skapa uttryck och använda dess metoder för att söka och manipulera text.

## Se även:
- [MDN Reguljära Uttryck Guide](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp-klassen i TypeScript Dokumentationen](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)