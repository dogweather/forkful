---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) är ett textbaserat dataformat för att spara och utbyta data. Programmerare använder JSON för dess enkelhet och för att det är lätt att läsa för både människor och maskiner.

## Hur gör man:
Här är ett exempel på hur du hanterar JSON i TypeScript:

```typescript
// Definiera en TypeScript-typ
interface Användare {
  namn: string;
  ålder: number;
  aktiv: boolean;
}

// Skapa ett JSON-strängexempel
const jsonSträng = '{"namn": "Anna", "ålder": 28, "aktiv": true}';

// Konvertera JSON-strängen till ett TypeScript-objekt
const användare: Användare = JSON.parse(jsonSträng);

console.log(användare.namn); // Skriver ut: Anna

// Konvertera TypeScript-objekt tillbaka till en JSON-sträng
const nyJsonSträng = JSON.stringify(användare);

console.log(nyJsonSträng); // Skriver ut: {"namn":"Anna","ålder":28,"aktiv":true}
```

## Fördjupning
JSON började användas i början av 2000-talet och är baserat på JavaScript-syntax, men är helt oberoende av språket. Alternativ till JSON inkluderar XML och YAML, men JSON är vanligtvis föredragen för dess kompakthet och hastighet. I TypeScript hanteras JSON via `JSON.parse()` och `JSON.stringify()` som omvandlar mellan JSON-strängar och TypeScript-objekt.

## Se även
- [MDN Web Docs om JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [JSON.org](https://www.json.org/json-en.html)