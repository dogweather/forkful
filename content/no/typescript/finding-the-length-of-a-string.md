---
title:                "Å finne lengden av en streng"
html_title:           "TypeScript: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Dette er en enkel oppgave som kan hjelpe deg med å håndtere og manipulere tekst på en mer effektiv måte. Hvis du for eksempel trenger å klippe av en del av en tekst, eller sammenligne to tekststrenger, så er det nyttig å kunne finne lengden på dem. Ved å lære dette vil du kunne skrive mer effektiv og pålitelig kode.

## Hvordan
For å finne lengden på en tekststreng i TypeScript, kan du bruke `.length` metoden. Her er et eksempel på hvordan du kan finne lengden på en tekststreng og skrive ut resultatet:

```TypeScript
let navn: string = "Kari";
console.log(navn.length); // output: 4
```

Hvis du ønsker å finne lengden på en tekststreng som inneholder mellomrom eller spesialtegn, må du også ta hensyn til dem i lengden.

```TypeScript
let setning: string = "Hei på deg!";
console.log(setning.length); // output: 11
```

Legg merke til at mellomrommet og utropstegnet også teller med i lengden. Dette gjelder også for spesialtegn som æ, ø og å.

## Dypdykk
Når du bruker `.length` metoden på en tekststreng, vil du få tilbake lengden på strengen som et heltall (integer). Dette gjør det enkelt å sammenligne lengden på to tekststrenger ved hjelp av betingede uttrykk. For eksempel, hvis du ønsker å sjekke om en tekststreng er lengre enn en annen, kan du bruke følgende kode:

```TypeScript
let tekst1: string = "heisann";
let tekst2: string = "hallo";

if (tekst1.length > tekst2.length) {
  console.log("Tekst1 er lengre enn tekst2");
} else {
  console.log("Tekst1 er ikke lengre enn tekst2");
}
```

Du kan også bruke `.length` metoden til å klippe av deler av en tekst, ved hjelp av `.slice()` metoden. Ved å angi start- og sluttpunktet for hvor du vil klippe teksten, kan du enkelt få tak i en del av strengen. Her er et eksempel på hvordan du kan klippe ut de to første bokstavene i en tekststreng og skrive dem ut:

```TypeScript
let tekst: string = "Hei på deg";
console.log(tekst.slice(0,2)); // output: He
```

## Se også
- [TypeScript dokumentasjon for strenger](https://www.typescriptlang.org/docs/handbook/strings.html)
- [W3Schools - String length property](https://www.w3schools.com/jsref/jsref_length_string.asp)