---
date: 2024-01-26 04:18:45.538609-07:00
description: "Hur g\xF6r man: TypeScript kommer inte med sin egen REPL. L\xE5t oss\
  \ anv\xE4nda `ts-node`, en TypeScript-exekveringsmilj\xF6 f\xF6r Node.js som inkluderar\
  \ en REPL.\u2026"
lastmod: '2024-03-13T22:44:37.657708-06:00'
model: gpt-4-0125-preview
summary: TypeScript kommer inte med sin egen REPL.
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur gör man:
TypeScript kommer inte med sin egen REPL. Låt oss använda `ts-node`, en TypeScript-exekveringsmiljö för Node.js som inkluderar en REPL.

Först, installera den globalt:
```bash
npm install -g ts-node
```

Starta REPL genom att skriva `ts-node` i din kommandorad:
```bash
ts-node
```

Här är en snabb snutt att prova:
```TypeScript
> let message: string = 'Hej, REPL!';
> console.log(message);
Hej, REPL!
> 
```
För att avsluta sessionen, tryck `Ctrl+D`.

## Djupdykning
Historiskt sett var REPLs framstående i språk som Lisp, vilket tillät dynamisk kodutvärdering. Konceptet har sedan spridits och blivit en grundpelare för interaktiv kodning i många språk.

För TypeScript är `ts-node` inte ditt enda alternativ. Alternativ inkluderar att använda TypeScript Playground i en webbläsare eller att utnyttja andra Node.js-baserade REPLs som stödjer TypeScript med lämpliga plugins.

När det gäller implementering använder `ts-node` TypeScript-kompilatorns API för att transpilera kod "on-the-fly" innan den exekveras av Node.js. Detta ger dig omedelbar feedback och är särskilt användbart för att prova de senaste funktionerna i TypeScript utan installationskrångel.

En sak att komma ihåg – medan en REPL är fantastisk för snabba tester, ersätter den inte att skriva traditionell, testbar och underhållbar kod. Det är ett verktyg för lärande och utforskning, inte ett substitut för korrekta utvecklingspraxis.

## Se också
- [TypeScripts officiella webbplats](https://www.typescriptlang.org/)
- [ts-node på GitHub](https://github.com/TypeStrong/ts-node)
- [Node.js REPL-dokumentation](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
