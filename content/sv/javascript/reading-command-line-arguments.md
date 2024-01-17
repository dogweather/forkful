---
title:                "Läsning av kommandoradsargument"
html_title:           "Javascript: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Vad & Varför?
Att läsa kommandoradsargument är en vanlig uppgift för JavaScript-programmerare. Det gör det möjligt för programmet att ta emot input från användaren när det körs, vilket kan vara användbart för att anpassa programmets beteende och hantera olika scenarier.

Hur man gör:
För att läsa kommandoradsargument kan vi använda Node.js process.argv-objektet. Detta objekt innehåller en array av argument som skickas till programmet från kommandoraden. Här är en enkel kodexempel som skriver ut alla de givna argumenten i konsolen:

```Javascript
process.argv.forEach((arg, index) => {
  console.log(`Argument #${index}: ${arg}`);
});

```

När du kör detta program med kommandoradsargument som "node script.js hello world", kommer du att se följande output:

```
Argument #0: node
Argument #1: script.js
Argument #2: hello
Argument #3: world
```

För att endast få tag på vissa argument kan vi använda indexering på arrayen, till exempel `process.argv[2]` för att få det första argumentet efter filnamnet.

Djupdykning:
Att läsa kommandoradsargument har funnits sedan de tidiga dagarna av UNIX-system och är en vanlig funktion i många programmeringsspråk. Alternativet till att använda `process.argv` är att använda ett externa bibliotek som `commander` som gör det enklare att hantera olika argument och switchar.

Se även:
- [Node.js - Process](https://nodejs.org/api/process.html#process_process_argv)
- [Commander](https://www.npmjs.com/package/commander)