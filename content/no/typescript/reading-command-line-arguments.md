---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:57.680744-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter lar programmereren hente input direkte fra brukerens terminal. Det er nyttig for skript og applikasjoner som krever dynamisk input, og for å tilpasse hvordan et program oppfører seg.

## Hvordan:
```TypeScript
import { argv } from 'process';

// Et enkelt eksempel på å lese kommandolinjeargumenter
const args = argv.slice(2);  // Ignorerer de to første elementene i 'argv'

console.log('Argumenter mottatt:');
args.forEach((arg, index) => {
  console.log(`${index}: ${arg}`);
});

// Kjøre scriptet med: ts-node script.ts arg1 arg2 ...
```
Sample output for `ts-node script.ts Hei Verden`:
```
Argumenter mottatt:
0: Hei
1: Verden
```

## Dypdykk
Lesing av kommandolinjeargumenter i TypeScript bygger på JavaScripts evner siden TypeScript kompileres til JavaScript. Historisk sett har `process.argv` fra Node.js miljøet vært standardmetoden for å nå disse argumentene. Det finnes alternativer som `commander.js` og `yargs` som tilbyr mer avansert funksjonalitet som argumentvalidering og parsing. Implementeringsdetaljer inkluderer vanligvis å håndtere innganger som flags, key-value pairs, og validere dem mot forventet format.

## Se Også
- Node.js dokumentasjon på `process.argv`: https://nodejs.org/docs/latest-v16.x/api/process.html#processargv
- `commander.js` GitHub-repositorium: https://github.com/tj/commander.js/
- `yargs` hjemmeside: https://yargs.js.org/
