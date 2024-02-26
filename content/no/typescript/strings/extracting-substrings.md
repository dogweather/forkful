---
date: 2024-01-20 17:46:40.079223-07:00
description: "\xC5 hente ut understrenger betyr \xE5 ta ut en spesifisert del av en\
  \ streng. Programmerere gj\xF8r dette for \xE5 bearbeide data, validere input eller\
  \ simpelthen\u2026"
lastmod: '2024-02-25T18:49:38.701026-07:00'
model: gpt-4-1106-preview
summary: "\xC5 hente ut understrenger betyr \xE5 ta ut en spesifisert del av en streng.\
  \ Programmerere gj\xF8r dette for \xE5 bearbeide data, validere input eller simpelthen\u2026"
title: Uthenting av delstrenger
---

{{< edit_this_page >}}

## What & Why?
Å hente ut understrenger betyr å ta ut en spesifisert del av en streng. Programmerere gjør dette for å bearbeide data, validere input eller simpelthen skille ut relevant informasjon.

## How to:
```TypeScript
let fullString: string = "Hallo, verden!";
let substring: string = fullString.substring(7, 13);
console.log(substring); // Output: "verden"
```

Eller, med moderne JavaScript/TypeScript, kan du bruke string literal templates:

```TypeScript
let user = "Ola";
let message: string = `Hei, ${user}!`;
console.log(message.substring(4)); // Output: "Ola!"
```

## Deep Dive
Før ES6 og TypeScript, var `substring()` og `slice()` de mest brukte metodene for å hente ut understrenger. Men, disse metodene oppfører seg litt annerledes: `slice()` kan ta negative indekser, mens `substring()` ikke kan det.

Alternativt kan du bruke `substr()`, men denne metoden er nå foreldet og bør unngås.

At TypeScript tillater streng-interpolasjon og har sterk typetøtte gjør jobben med strenger lettere og mer feilsikker.

## See Also
- MDN på `substring()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- TypeScript Docs: https://www.typescriptlang.org/docs/
- ECMAScript 2015 (ES6) spesifikasjoner: https://www.ecma-international.org/ecma-262/6.0/
