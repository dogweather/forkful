---
title:                "Extrahering av delsträngar"
html_title:           "TypeScript: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar är en vanlig uppgift inom programmering där man tar en del av en sträng och använder den för olika ändamål. Det kan till exempel vara för att hitta och ersätta vissa ord eller för att filtrera och sortera data. Genom att kunna extrahera delsträngar kan man göra textbehandling och datahantering mer effektiv.

## Så här:
```TypeScript
// Exempel 1: Extrahera en del av en sträng
const str = "Hej, jag heter Emma och jag älskar att programmera!"

const name = str.substring(10, 14);

console.log(name); // "Emma"

// Exempel 2: Byt ut en del av en sträng
const newStr = str.replace("Emma", "Lisa");

console.log(newStr); // "Hej, jag heter Lisa och jag älskar att programmera!"
```

## Djupdykning:
I många moderna programmeringsspråk, som TypeScript, finns det inbyggda funktioner för att extrahera delsträngar. Det är dock en funktion som funnits långt tillbaka i tiden, och i äldre språk måste man ofta skriva mer komplex kod för att åstadkomma samma sak. Alternativt kan man använda en så kallad "regex", en sträng som beskriver ett mönster, för att hitta och extrahera delar av en annan sträng.

## Se även:
- [TypeScript dokumentation för substrings](https://www.typescriptlang.org/docs/handbook/strings.html#substring-and-substr)
- [Regex tutorial för att extrahera data](https://www.regular-expressions.info/extract.html)