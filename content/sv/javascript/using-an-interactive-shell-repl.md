---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:15:44.685815-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interaktiva skal, eller REPL (Read-Eval-Print Loop), låter dig köra kod på flyget, testa funktioner, algoritmer eller pilla med idéer. De är kodningens klotterplank, snabba och smutsiga, utan att behöva sätta upp en hel utvecklingsmiljö.

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
