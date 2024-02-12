---
title:                "Använda en interaktiv skal (REPL)"
aliases:
- /sv/typescript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:45.538609-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En Read-Eval-Print-Loop (REPL) är en programmeringsmiljö som tar emot enskilda användarinmatningar, exekverar dem och returnerar resultatet till användaren. Programmerare använder en REPL för att snabbt experimentera med kodsnuttar, felsöka och lära sig nya språkfunktioner utan det extra arbete som krävs för att skapa en fullständig applikation.

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
