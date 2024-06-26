---
date: 2024-01-26 04:11:26.627248-07:00
description: "Hvordan: For \xE5 komme i gang med en debugger i TypeScript, trenger\
  \ du bare et st\xF8ttet IDE (som Visual Studio Code) og en `launch.json`-konfigurasjon.\
  \ Her\u2026"
lastmod: '2024-03-13T22:44:40.536908-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 komme i gang med en debugger i TypeScript, trenger du bare et st\xF8\
  ttet IDE (som Visual Studio Code) og en `launch.json`-konfigurasjon."
title: "\xC5 bruke en debugger"
weight: 35
---

## Hvordan:
For å komme i gang med en debugger i TypeScript, trenger du bare et støttet IDE (som Visual Studio Code) og en `launch.json`-konfigurasjon. Her er et kjapt eksempel for en Node.js-applikasjon:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hei, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

For å feilsøke dette, opprett en `launch.json`-fil under `.vscode`-mappen:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Start program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Deretter setter du et brytepunkt i din `greet`-funksjon ved å klikke på venstre side av linjenummeret i IDEen din. Trykk F5 for å starte feilsøkingen, og se at appen din pauser ved brytepunktet. Du kan nå holde musepekeren over variabler, følge med på uttrykk og stegvis gå gjennom koden din med letthet.

## Dypdykk
Tilbake i tiden før integrerte utviklingsmiljøer (IDEer) ble glatte, ble feilsøking ofte gjort med utskriftsuttrykk (kjent som `console.log`-feilsøking). Det fungerte, på en måte, men det var som å prøve å finne en nål i en høystakk med bind for øynene.

Moderne feilsøkere er som en sveitserkniv for feilsøking. Med utviklingen av TypeScript og Node.js finnes det ulike feilsøkere tilgjengelig, fra den innebygde Node.js-inspektøren til nettleserutviklerverktøy for feilsøking på klientsiden.

Node.js-inspektøren fungerer ved å koble seg til din kjørende applikasjon; den kommuniserer over Chrome DevTools-protokollen, noe som gjør Chrome-nettleseren din til en kraftig feilsøkingskonsoll. Denne integrasjonen tillater en visuelt interaktiv og detaljert feilsøkingssesjon, sammenlignet med tradisjonelle kommandolinje-feilsøkingspraksiser.

## Se også
For litt ekstra lesing og noen pro-tips, sjekk ut:

- [TypeScript-feilsøking i Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js-feilsøkingsguide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Dokumentasjon for Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
