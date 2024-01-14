---
title:                "Javascript: Läsning av kommandoradsargument"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Om du programmerar i Javascript, kan du ha hört talas om kommandoradsargument. Men varför är det viktigt att lära sig att läsa dem? Kommandoradsargument kan vara en kraftfull funktion som kan hjälpa dig att skriva mer dynamiska och interaktiva program. Låt oss titta närmare på hur man kan använda dem.

## Hur man gör

För att läsa kommandoradsargument i Javascript, behöver du först tillgång till processobjektet. Detta objekt innehåller information om det körs från en kommandorad. Sedan kan du använda metoden `process.argv` för att läsa in argumenten som tillhandahålls när programmet körs.

```Javascript
// Exempel på hur man kan läsa kommandoradsargument
const argument = process.argv[2]; // argumentet efter "node filnamn.js"
console.log("Du angav argumentet: " + argument);
```

Om du till exempel har ett program som heter "greeting.js" och du kör det från kommandoraden med kommandot `node greeting.js Hello`, kommer utmatningen att vara `Du angav argumentet: Hello`.

## Djupdykning

När du läser argumenten kan du märka att de är i form av en array, där det första elementet är sökvägen till den körda filen och de efterföljande elementen är de argument som tillhandahålls. Du kan också använda olika metoder för att bearbeta och manipulera argumenten på olika sätt.

Kommandoradsargument kan vara användbara när du vill göra ditt program mer interaktivt genom att möjliggöra att användaren tillhandahåller olika värden vid körning. Du kan också använda dem för att göra ditt program mer anpassningsbart, till exempel genom att låta användaren ange sökvägen till en viss fil eller liknande.

## Se också

- [Node.js process.argv documentation](https://nodejs.org/docs/latest-v12.x/api/process.html#process_process_argv)
- [Commander.js library for parsing command line options](https://www.npmjs.com/package/commander)
- [Readline-sync library for interactive command line prompts](https://www.npmjs.com/package/readline-sync)