---
title:                "Sammenføyning av tekststrenger"
html_title:           "TypeScript: Sammenføyning av tekststrenger"
simple_title:         "Sammenføyning av tekststrenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger kan være nyttig når du ønsker å lage dynamiske setninger, som inkluderer variabler eller brukerinput. Dette gjør det enkelt å lage mer dynamisk og tilpasningsdyktig kode.

## Hvordan Kombinere Strenger i TypeScript

I TypeScript, kan du kombinere strenger på to måter: ved hjelp av "+" operatøren eller ved hjelp av "Template Strings". La oss se på et eksempel på begge metodene:

```TypeScript
let navn: string = "Nina";
let alder: number = 25;

// Ved hjelp av "+" operatøren
let setningA: string = "Hei, mitt navn er " + navn + ", og jeg er " + alder + " år gammel.";

// Ved hjelp av "Template Strings"
let setningB: string = `Hei, mitt navn er ${navn}, og jeg er ${alder} år gammel.`;

console.log(setningA);
console.log(setningB);

/*
Setning A output: Hei, mitt navn er Nina, og jeg er 25 år gammel.
Setning B output: Hei, mitt navn er Nina, og jeg er 25 år gammel.
*/

```

Som du kan se, produserer begge metodene samme output. Forskjellen er at "Template Strings" gjør det enklere å inkludere variabler i en streng, ved å bruke ${} syntaks rundt variabelnavnet.

## Dykk Dypere Inn

Nå som vi har sett på de to måtene å kombinere strenger på, la oss se på noen flere ting å huske på når du jobber med dette i TypeScript.

- Kan også kombinere strenger med += operatøren: I tillegg til å bruke + operatøren, kan du også bruke += operatøren for å legge til en streng på slutten av en eksisterende streng. For eksempel: `let setning: string = "Dette er en " + "test.";` kan også skrives som `let setning: string = "Dette er en "; setning += "test.";`.

- Bruk av backslash (\): Hvis du ønsker å inkludere spesialtegn, som for eksempel et anførselstegn, i en kombinert streng, kan du bruke backslash (\) foran tegnet. Dette signaliserer at det er en del av strengen og ikke en del av syntaksen. For eksempel: `let setning: string = "Dette er et \"anførselstegn\".";`.

## Se Også

- [Offisiell TypeScript Dokumentasjon - Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [JavaScript.info - Strings](https://javascript.info/types#string)
- [MDN Web Docs - Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)