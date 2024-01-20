---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Jobbing med JSON (JavaScript Object Notation) handler om å utveksle data mellom server og klient, eller mellom programmer. Programmerere bruker JSON fordi det er lett å lese og skrive, godt støttet, og det bruker en standard format som kan tolkes av de fleste programmeringsspråk.

## Hvordan gjøre det:

```typescript
// Definerer en TypeScript-interface for type-sikkerhet
interface Bruker {
  navn: string;
  alder: number;
  aktiv: boolean;
}

// Et eksempel på en JSON-string
const jsonStr: string = '{"navn":"Ola","alder":30,"aktiv":true}';

// Konverter JSON-string til TypeScript-objekt
const brukerObj: Bruker = JSON.parse(jsonStr);

console.log(brukerObj.navn); // Utskrift: Ola

// Konverter TypeScript-objekt tilbake til JSON-string
const nyJsonStr: string = JSON.stringify(brukerObj);

console.log(nyJsonStr); // Utskrift: '{"navn":"Ola","alder":30,"aktiv":true}'
```

## Dypdykk

JSON eksisterte først som en del av JavaScript, men det har siden blitt et selvstendig dataformat som brukes på tvers av mange språk og plattformer. Alternativer til JSON inkluderer XML og YAML, men JSON er ofte foretrukket for dets enkelhet og hastighet. Implementeringsdetaljer inkluderer parsing og stringifying, som kan håndtere komplekse objekter, inkludert datoer og spesialtegn, som krever forsiktig behandling for å unngå feil.

## Se også

- MDN Web Docs JSON: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- TypeScript Handbook: [https://www.typescriptlang.org/docs/handbook/intro.html](https://www.typescriptlang.org/docs/handbook/intro.html)
- json.org: [http://json.org/](http://json.org/)