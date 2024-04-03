---
date: 2024-01-20 17:56:32.490887-07:00
description: "Hur man g\xF6r: L\xE5t oss dyka rakt in i koden. S\xE5 h\xE4r kan du\
  \ l\xE4sa argument fr\xE5n kommandoraden i Node.js."
lastmod: '2024-03-13T22:44:38.308752-06:00'
model: gpt-4-1106-preview
summary: "L\xE5t oss dyka rakt in i koden."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Hur man gör:
Låt oss dyka rakt in i koden. Så här kan du läsa argument från kommandoraden i Node.js:

```javascript
// example.js
// Använd process.argv för att hämta kommandoradsargument
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// Kör programmet med: node example.js arg1 arg2 arg3
```

Kör följande i terminalen för att testa:

```shell
node example.js hej hallå tja
```

Förväntad utskrift:

```shell
0: /path/to/node
1: /path/to/your/example.js
2: hej
3: hallå
4: tja
```

Endast index 2 och framåt är dina egna argument.

## Djupdykning:
Kommandoradsargument har använts sedan urminnes tider. I Node.js förlitar vi oss på `process.argv`, en array där index 0 är sökvägen till Node.js-binären och index 1 är den körande filens sökväg. 

Alternativ för att göra detta snyggare är bland annat `yargs` eller `commander`, moduler som hanterar komplexa argument på ett strukturerat sätt. Vid implementering kan det vara klokt att välja dessa moduler för bättre felsäkerhet och enklare kod.

Vidare kan funktioner som `process.argv.slice(2)` användas för att direkt hoppa till de relevanta argumenten. Och för mer komplexa program kan du använda environment-variabler för att styra beteendet snarare än kommandoradsargument.

## Se även:
Här är några länkar för vidare läsning och utforskning:

- Node.js dokumentation om process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Yargs GitHub-sida: https://github.com/yargs/yargs
- Commander GitHub-sida: https://github.com/tj/commander.js

Med dessa i ryggen är du väl rustad att handskas med användarinput i dina kommandoradsprogram – lycka till!
