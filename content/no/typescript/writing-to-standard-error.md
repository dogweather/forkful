---
title:                "TypeScript: Skriver til standardfeil"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig del av programmering, spesielt når det kommer til feilsøking og debugging. Ved å skrive til standard error kan man få informasjon om feil og unntak som kan hjelpe med å finne og rette opp i problemer i koden.

## Hvordan

For å skrive til standard error i TypeScript kan man bruke funksjonen console.error(). Dette vil skrive ut en melding til standard error strømmen, som vanligvis vises i en annen farge eller på en annen linje enn standard output. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```TypeScript
console.error("Det har oppstått en feil!"); // Utskrift: Det har oppstått en feil!
```

Man kan også inkludere variabler eller objekter i meldingen ved å bruke template literals. Dette kan være nyttig for å få mer spesifikk informasjon om feilen. Her er et eksempel på hvordan dette kan gjøres:

```TypeScript
let num1 = 10;
let num2 = 0;
console.error(`Det har oppstått en feil ved divisjon av ${num1} med ${num2}.`); // Utskrift: Det har oppstått en feil ved divisjon av 10 med 0.
```

## Dypdykk

Når man bruker console.error() vil meldingen vanligvis vises i konsollen i nettleseren eller i terminalen, avhengig av hvor koden kjører. Det er også mulig å omdirigere standard error strømmen til en fil for å lagre feilmeldingene. Dette kan gjøres ved hjelp av node.js sin predefinerte standard error strøm process.stderr.

Det kan også være nyttig å håndtere feilmeldinger ved å bruke try-catch blokker. Dette vil fange eventuelle unntak som oppstår og kan hjelpe med å håndtere dem på en mer effektiv måte.

## Se også

- [Node.js Documentation - process.stderr](https://nodejs.org/api/process.html#process_process_stderr)
- [MDN web docs - console.error()](https://developer.mozilla.org/en-US/docs/Web/API/console/error)