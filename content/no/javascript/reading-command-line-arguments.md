---
title:                "Lese kommandolinjeargumenter"
aliases:
- no/javascript/reading-command-line-arguments.md
date:                  2024-01-20T17:56:36.402643-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Kommandolinjeargumenter lar oss gi input direkte til et JavaScript-program i kjøretid, slik at oppførselen kan endres dynamisk. Programmerere bruker dette for å gjøre skriptene mer fleksible og for å kunne håndtere ulike scenarier uten å røre koden.

## How to:
For å lese kommandolinjeargumenter i JavaScript, bruker vi ofte `process.argv`, som er en array i Node.js. Her er et enkelt eksempel:

```javascript
// file: greet.js
console.log(`Hei, ${process.argv[2]}!`);

// Kjør skriptet med: node greet.js Verden
// Forventet output: Hei, Verden!
```

Koden over tar et navn fra kommandolinjen og setter det inn i en hilsen.

## Deep Dive
Før Node.js ble populært, var JavaScript mest brukt for å kjøre i nettlesere - uten tilgang til systemets kommandolinje. Med Node.js kan JavaScript kjøre på server-siden eller som lokale skript, der det å lese fra kommandolinjen er nyttig.

Det finnes andre metoder enn `process.argv` for å håndtere argumenter, som f.eks. biblioteker som `yargs` eller `commander` som gir mer funksjonalitet og lettere syntaks.

Prosessen går slik:
1. `process.argv` inneholder en array med kommandolinjeinnganger. Første element er 'node', det andre er filstien til skriptet, og fra det tredje elementet er det faktiske argumenter.
2. Du kan løkke gjennom `process.argv` for å håndtere flere argumenter.
3. Biblioteker som `yargs` tolker argumenter for deg og kan håndtere ting som flagg og standardverdier.

## See Also
- Node.js-dokumentasjon på kommandolinje-argumenter: [Node.js process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `yargs` GitHub-side for mer avansert argumenthåndtering: [yargs/yargs](https://github.com/yargs/yargs)
- `commander` GitHub-side, et annet populært valg for kommandolinjeargument-biblioteker: [tj/commander.js](https://github.com/tj/commander.js)
