---
date: 2024-01-20 17:33:53.505620-07:00
description: "J\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att best\xE4mma deras relation\
  \ - \xE4r ett tidigare, senare eller samma dag. Programmerare g\xF6r detta f\xF6\
  r att hantera tidsbaserade\u2026"
lastmod: '2024-03-13T22:44:37.668467-06:00'
model: gpt-4-1106-preview
summary: "J\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att best\xE4mma deras relation - \xE4\
  r ett tidigare, senare eller samma dag. Programmerare g\xF6r detta f\xF6r att hantera\
  \ tidsbaserade\u2026"
title: "J\xE4mf\xF6ra tv\xE5 datum"
---

{{< edit_this_page >}}

## What & Why? - "Vad & Varför?"
Jämföra två datum innebär att bestämma deras relation - är ett tidigare, senare eller samma dag. Programmerare gör detta för att hantera tidsbaserade händelser, som utgångsdatum, schemaläggning eller att beräkna tidsintervaller.

## How to: - "Hur gör man:"
```TypeScript
const date1: Date = new Date('2023-04-01T00:00:00');
const date2: Date = new Date('2023-04-01T12:00:00');
const date3: Date = new Date('2023-04-02T00:00:00');

// Jämför två datum: Är de lika?
console.log(date1.getTime() === date2.getTime()); // Output: false

// Är date1 tidigare än date3?
console.log(date1 < date3); // Output: true

// Är date2 senare än date1?
console.log(date2 > date1); // Output: true

// Skriv ut skillnaden i millisekunder
console.log(date3.getTime() - date2.getTime()); // Output: 43200000 (12 timmar i millisekunder)
```

## Deep Dive - "Djupdykning"
Historiskt sett hanteras datum i programmering genom att räkna antalet millisekunder sedan 'Epok', som startade vid midnatt 1 januari 1970 UTC. I TypeScript använder vi `Date`-objektet för att skapa och hantera datum. 

Det finns alternativ till att jämföra datum med `getTime()`. Vi kan använda `valueOf()` eller operatorer som `>` och `<`, vilka implicit använder `valueOf()`. Implementationen av `Date` i JavaScript (och därigenom TypeScript) kan ha varierande resultat beroende på tidszon och webbläsarimplementering.

## See Also - "Se även"
- MDN Web Docs om `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- TypeScript officiella dokumentation: https://www.typescriptlang.org/docs/
- Tidszonshantering med `moment-timezone`: https://momentjs.com/timezone/
