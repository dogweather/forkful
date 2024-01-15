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

## Varför?

Är du nyfiken på hur man kan extrahera substrängar i TypeScript? Det kan hjälpa dig att manipulera textsträngar och få ut specifika delar av en sträng, vilket kan vara användbart för att till exempel bearbeta användardata eller filtrera data.

## Så här gör du

För att extrahera substrängar i TypeScript behöver du använda en inbyggd metod som heter `substring()`. Den tar in två parametrar, startindex och slutindex, och returnerar en ny sträng med de tecken som finns mellan dessa index.

```TypeScript
let str = "JavaScript är ett roligt programmeringsspråk";

// Extrahera "JavaScript" från strängen
let substring = str.substring(0, 10);

console.log(substring); // Output: JavaScript
```

Om du bara anger startindexet kommer `substring()` att extrahera alla tecken från och med det indexet till slutet av strängen.

```TypeScript
let str = "JavaScript är ett roligt programmeringsspråk";

// Extrahera "är ett roligt programmeringsspråk" från strängen
let substring = str.substring(11);

console.log(substring); // Output: är ett roligt programmeringsspråk
```

Om du, istället för att använda index, vill extrahera en del av en sträng baserat på antalet tecken så kan du använda `substr()` metoden istället.

```TypeScript
let str = "Welcome to TypeScript";

// Extrahera "TypeScript" från strängen
let substring = str.substr(11, 10); // startindex: 11, antal tecken: 10

console.log(substring); // Output: TypeScript
```

Du kan även använda metoden `slice()` för att extrahera substrängar i TypeScript.

```TypeScript
let str = "Hello world";

// Extrahera "world" från strängen
let substring = str.slice(6);

console.log(substring); // Output: world
```

## Deep Dive

Att extrahera substrängar är en vanlig användning av textsträngar i programmering. Med hjälp av `substring()`, `substr()` eller `slice()` metoder kan du välja vilka delar av en sträng som ska användas, beroende på vad som passar bäst för ditt specifika projekt.

Det är också värt att nämna att strängar i TypeScript är noll-indexerade, vilket betyder att det första tecknet i en sträng har index 0. Detta kan vara viktigt att känna till när du använder de olika metoderna för att extrahera substrängar.

## Se även

Här är några användbara resurser för att lära dig mer om att extrahera substrängar i TypeScript:

- [MDN webbdokumentation om substring()](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [stackoverflow.com - Fråga om att extrahera substrängar i TypeScript](https://stackoverflow.com/questions/55169828/how-to-extract-substring-in-typescript)
- [TypeScript dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)