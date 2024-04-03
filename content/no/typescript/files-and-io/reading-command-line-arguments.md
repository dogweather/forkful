---
date: 2024-01-20 17:56:57.680744-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:40.547528-06:00'
model: gpt-4-1106-preview
summary: .
title: Lese kommandolinjeargumenter
weight: 23
---

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
