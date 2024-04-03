---
date: 2024-01-26 04:15:44.685815-07:00
description: "Hur man g\xF6r: Node.js levereras med ett REPL som \xE4r tillg\xE4ngligt\
  \ via terminalen. Poppa upp det, och du \xE4r redo att b\xF6rja. H\xE4r \xE4r en\
  \ smakprov."
lastmod: '2024-03-13T22:44:38.295006-06:00'
model: gpt-4-0125-preview
summary: "Node.js levereras med ett REPL som \xE4r tillg\xE4ngligt via terminalen."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
Node.js levereras med ett REPL som är tillgängligt via terminalen. Poppa upp det, och du är redo att börja. Här är en smakprov:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Rättfram, eller hur? Definiera variabler, funktioner, eller kör loopar. När du är klar tar `.exit` dig tillbaka till verkligheten.

## Djupdykning
REPL har funnits sedan 1960-talet – LISP var pionjär för konceptet. Idén: ge omedelbar återkoppling till programmeraren. Alternativ? Förutom Node.js REPL finns det webbläsarbaserade konsoler som Chrome DevTools, online-sandlådor som JSFiddle, eller fullständiga IDEer som VSCode med interaktiva lekplatser.

Under huven följer REPL-arbetsflöden vanligtvis:
1. Läs inmatning
2. Kompilera och exekvera kod
3. Skriv ut output
4. Loopa tillbaka

Det är en enkel men ändå effektiv cykel som har haft enorm påverkan på interaktiv kodning.

## Se också
- [Dokumentation för Node.js REPL](https://nodejs.org/api/repl.html)
- [Mozillas introduktion till JavaScript-moduler på REPLs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
