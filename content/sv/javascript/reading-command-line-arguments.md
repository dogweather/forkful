---
title:    "Javascript: Inläsning av kommandoradsargument"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att kunna läsa kommandoradsargument är en viktig del av JS-programmering. Det tillåter dig att interagera med ditt program och göra justeringar på ett snabbt och effektivt sätt.

## Hur man gör det 
För att läsa kommandoradsargument i JS, bör du först skapa en variabel som lagrar process.argv. Detta är ett inbyggt objekt som innehåller en matris av alla argument som skickats till ditt program vid körning. Du kan sedan använda variabeln i ditt program för att hämta och använda de specifika argumenten som behövs. Till exempel:

```
// Skapa en variabel för kommandoradsargumentet
var commandLineArgument = process.argv;

// Hämta det första argumentet efter filnamnet
var arg1 = commandLineArgument[2];

// Hämta det andra argumentet efter filnamnet
var arg2 = commandLineArgument[3];

// Skriv ut en kombination av de två argumenten
console.log("Argumenten tillsammans blir: " + arg1 + arg2);
```

Om du kör programmet med kommandot `node index.js hello world`, skulle konsolen visa "Argumenten tillsammans blir: hello world".

## Djupdykning
När du läser kommandoradsargument i JS är det viktigt att du förstår hur de fungerar. Process.argv-objektet är en matris där det första argumentet är sökvägen till programmet, det andra argumentet är sökvägen till filen som används för att köra programmet, och resten av argumenten är de som skickas till programmet. Om du vill lära dig mer om hur kommandoradsargument fungerar i detalj kan du kolla in [denna guide](https://nodejs.org/en/knowledge/command-line/how-to-parse-command-line-arguments/) från Node.js dokumentationen.

## Se även
- [Använda kommandoradsargument i Node.js](https://nodejs.org/en/knowledge/command-line/how-to-parse-command-line-arguments/)
- [Argv-objektet](https://nodejs.org/api/process.html#process_process_argv) (Node.js dokumentation)