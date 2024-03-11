---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:37.619017-07:00
description: "Een Read-Eval-Print-Loop (REPL) is een programmeeromgeving die individuele\
  \ gebruikersinvoeren neemt, deze uitvoert en het resultaat aan de gebruiker\u2026"
lastmod: '2024-03-11T00:14:24.368304-06:00'
model: gpt-4-0125-preview
summary: "Een Read-Eval-Print-Loop (REPL) is een programmeeromgeving die individuele\
  \ gebruikersinvoeren neemt, deze uitvoert en het resultaat aan de gebruiker\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
Een Read-Eval-Print-Loop (REPL) is een programmeeromgeving die individuele gebruikersinvoeren neemt, deze uitvoert en het resultaat aan de gebruiker teruggeeft. Programmeurs gebruiken een REPL om snel met codefragmenten te experimenteren, te debuggen en nieuwe taalfuncties te leren zonder de overhead van het creëren van een volledige applicatie.

## Hoe te gebruiken:
TypeScript komt niet met zijn eigen REPL. Laten we `ts-node` gebruiken, een TypeScript uitvoeromgeving voor Node.js die een REPL bevat.

Installeer het eerst wereldwijd:
```bash
npm install -g ts-node
```

Start de REPL door `ts-node` in je opdrachtregel te typen:
```bash
ts-node
```

Hier is een snelle snippet om te proberen:
```TypeScript
> let message: string = 'Hallo, REPL!';
> console.log(message);
Hallo, REPL!
>
```
Om de sessie te beëindigen, druk op `Ctrl+D`.

## Diepgaande Duik
Historisch gezien waren REPLs prominent aanwezig in talen zoals Lisp, wat dynamische code-evaluatie toeliet. Het concept is sindsdien verspreid en is een basis geworden voor interactief programmeren in veel talen. 

Voor TypeScript is `ts-node` niet je enige optie. Alternatieven zijn onder meer het gebruik van de TypeScript Playground in een webbrowser of het benutten van andere Node.js-gebaseerde REPLs die TypeScript ondersteunen met geschikte plugins.

Wat implementatie betreft, gebruikt `ts-node` de TypeScript compiler API om code ter plekke te transpileren voordat het door Node.js wordt uitgevoerd. Dit geeft je directe feedback en is vooral handig om de nieuwste functies van TypeScript uit te proberen zonder opzetproblemen.

Eén ding om te onthouden – hoewel een REPL geweldig is voor snelle tests, vervangt het niet het schrijven van traditionele, testbare en onderhoudbare code. Het is een hulpmiddel voor leren en ontdekken, geen vervanging voor goede ontwikkelpraktijken.

## Zie Ook
- [Officiële website van TypeScript](https://www.typescriptlang.org/)
- [ts-node op GitHub](https://github.com/TypeStrong/ts-node)
- [Node.js REPL Documentatie](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
