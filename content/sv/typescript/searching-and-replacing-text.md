---
title:                "Sökning och ersättning av text"
html_title:           "TypeScript: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig del av programmering, särskilt när det kommer till att modifiera stora mängder data. Det kan vara ett effektivt sätt att ändra flera instanser av en specifik textsnutt på en gång.

## Så här gör du

Du kan använda TypeScript för att söka och ersätta text med hjälp av strängmetoder som .replace() eller reguljära uttryck. Med .replace() kan du ange den text som ska ersättas och den önskade ändringen, medan reguljära uttryck ger dig mer avancerade sök- och ersättningsmöjligheter.

```TypeScript
// Använda .replace()
let str = "Välkommen till TypeScript!";
let newStr = str.replace("Välkommen", "Hej"); // Hej till TypeScript!

// Använda reguljära uttryck
let str = "Jag älskar att kodas i TypeScript!";
let newStr = str.replace(/kodas i/g, "använda"); // Jag älskar att använda TypeScript!
```

## Fördjupning

När du använder reguljära uttryck för att söka och ersätta text kan du använda specialtecken för att göra sökningen mer specifik. Till exempel kan du använda "*" för att matcha alla tecken eller "+" för att matcha en eller flera repetitioner av ett tecken.

Du kan också använda flaggor för att ändra hur sökningen utförs, till exempel "g" för global sökning eller "i" för att ignorera skillnaden mellan gemener och versaler.

```TypeScript
let str = "Det finns bara ett sätt att lära sig TypeScript - övning!";
let newStr = str.replace(/[a-z]/gi, "?"); // ??? ????? ??? ???? ??? ????! 
```

## Se även

- [Dokumentation för strängmetoden .replace()](https://www.typescriptlang.org/docs/handbook/strings.html#search-and-replace)
- [Reguljära uttryck i TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)