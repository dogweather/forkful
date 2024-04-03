---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:51.052696-07:00
description: "Hoe te: Om aan de slag te gaan met een debugger in TypeScript, heb je\
  \ alleen een ondersteunde IDE (zoals Visual Studio Code) en een `launch.json`\u2026"
lastmod: '2024-03-13T22:44:50.555406-06:00'
model: gpt-4-0125-preview
summary: Om aan de slag te gaan met een debugger in TypeScript, heb je alleen een
  ondersteunde IDE (zoals Visual Studio Code) en een `launch.json` configuratie nodig.
title: Een debugger gebruiken
weight: 35
---

## Hoe te:
Om aan de slag te gaan met een debugger in TypeScript, heb je alleen een ondersteunde IDE (zoals Visual Studio Code) en een `launch.json` configuratie nodig. Hier is een snel voorbeeld voor een Node.js applicatie:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hallo, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Om dit te debuggen, maak je een `launch.json` bestand onder de `.vscode` map:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Programma Starten",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Vervolgens, stel je een breekpunt in je `greet` functie in door aan de linkerkant van het regelnummer in je IDE te klikken. Druk op F5 om het debuggen te starten, en bekijk hoe je app pauzeert bij het breekpunt. Je kunt nu over variabelen zweven, expressies bekijken en met gemak door je code stappen.

## Diepgaand
Terug in de tijd, voordat geïntegreerde ontwikkelomgevingen (IDE's) geavanceerd werden, werd debuggen vaak gedaan met printfuncties (ook wel bekend als `console.log` debuggen). Het werkte, soort van, maar was als het zoeken naar een naald in een hooiberg met een blinddoek voor.

Moderne debuggers zijn als een Zwitsers zakmes voor het oplossen van problemen. Met de evolutie van TypeScript en Node.js zijn er verschillende debuggers beschikbaar, van de ingebouwde Node.js-inspector tot browserontwikkeltools voor het debuggen aan de clientzijde.

De Node.js-inspector werkt door zich te verbinden met je draaiende applicatie; het communiceert via het Chrome DevTools-protocol, waardoor je Chrome-browser een krachtige debugconsole wordt. Deze integratie zorgt voor een visueel interactieve en gedetailleerde debugsessie, in vergelijking met traditionele opdrachtregel-debugpraktijken.

## Zie Ook
Voor wat extra leesvoer en enkele professionele tips, bekijk:

- [TypeScript Debuggen in Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js Debuggids](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools Documentatie](https://developers.google.com/web/tools/chrome-devtools)
