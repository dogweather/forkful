---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:25.508968-07:00
description: 'Hoe: In TypeScript gebruik je Node.js om opdrachtregelargumenten te
  lezen. Hier is hoe.'
lastmod: '2024-03-13T22:44:50.566745-06:00'
model: gpt-4-0125-preview
summary: In TypeScript gebruik je Node.js om opdrachtregelargumenten te lezen.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe:
In TypeScript gebruik je Node.js om opdrachtregelargumenten te lezen. Hier is hoe:

```typescript
// Nodig om process van Node.js te importeren
import process from 'process';

// Pak de opdrachtregelargumenten vanaf de derde positie verder
const args = process.argv.slice(2);

console.log('Opdrachtregelargumenten:', args);
```

Voer dit script uit als `ts-node yourscript.ts arg1 arg2` en zie:

```
Opdrachtregelargumenten: ['arg1', 'arg2']
```

## Diepgaand
Terug in de vroege dagen van de opdrachtregel was gebruikersinteractie helemaal gebaseerd op tekst. Linux, UNIX, en Windows gebruikten opdrachtregelargumenten om programma's te vertellen wat ze moesten doen.

Nu voor de alternatieven: naast `process.argv`, kun je in Node.js libraries zoals `yargs` of `commander` gebruiken voor meer functies zoals parsing en validatie.

Het wezen van dit in TypeScript is eenvoudig: `process.argv` is een array met alle argumenten. Index 0 is het pad naar Node, index 1 is het scriptpad, dus echte argumenten beginnen vanaf index 2.

## Zie Ook
Om verder te verkennen, begin met deze:

- [Node.js process.argv documentatie](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub repository](https://github.com/yargs/yargs)
- [Commander.js GitHub repository](https://github.com/tj/commander.js)
