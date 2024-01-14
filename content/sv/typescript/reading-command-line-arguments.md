---
title:                "TypeScript: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför läsa kommandoradsargument i TypeScript?

Att kunna läsa och hantera kommandoradsargument är en grundläggande färdighet för varje TypeScript-programmerare. Genom att förstå hur man läser och använder dessa argument kan du skriva mer flexibla program och interagera med användare på ett smidigare sätt. I denna blogginlägg kommer vi att ta en djupdykning i hur du läser kommandoradsargument i TypeScript och varför det är en viktig färdighet att ha.

## Hur man läser kommandoradsargument i TypeScript

För att läsa kommandoradsargument i TypeScript kan du använda objektet `process.argv`. Detta objekt innehåller en array av strängar som representerar de argument som skickats med vid körning av ditt program. Här är ett enkelt exempel på hur du kan använda detta objekt:

```TypeScript
const args: string[] = process.argv;

// Om det finns minst ett argument
if (args.length > 2) {
  // Det första argumentet är argument 2 i arrayen
  const firstArg: string = args[2];
  console.log(`Det första argumentet är: ${firstArg}`);
} else {
  console.log("Inga argument skickades med.");
}
```

När du kör detta program med kommandot `node index.js hej` kommer utmatningen att bli "Det första argumentet är: hej".

Det är viktigt att komma ihåg att index 0 och 1 i denna array är reserverade för den fullständiga sökvägen till node och sökvägen till ditt program. Så om du till exempel kör kommandot `node index.js` utan några argument, kommer längden på arrayen att vara 2 trots att du inte skickade med några argument.

## Djupdykning i kommandoradsargument i TypeScript

Utöver att bara läsa argumenten kan du även utföra specifika åtgärder beroende på vilka argument som skickas med. En vanlig användning av kommandoradsargument är att skicka in variabler vid körning av programmet. Med hjälp av dessa variabler kan du ändra beteendet hos ditt program utan att behöva ändra själva koden.

Låt oss ta ett exempel där vi vill kunna skicka med ett namn vid körning av programmet och sedan skriva ut en hälsning med detta namn. Här är hur vårt program kan se ut:

```TypeScript
const args: string[] = process.argv;

// Om det finns minst ett argument
if (args.length > 2) {
  // Hämta namnet
  const name: string = args[2];
  // Skriv ut hälsning
  console.log(`Hej ${name}! Välkommen till mitt program.`);
} else {
  console.log("Inga argument skickades med.");
}
```

När vi kör det här programmet med kommandot `node index.js Sebastian` kommer utmatningen att bli "Hej Sebastian! Välkommen till mitt program."

Det kan också vara användbart att skicka med en flagga för att kunna aktivera eller inaktivera en viss funktion i ditt program. Du kan till exempel ha en flagga för att aktivera eller inaktivera ett debug-läge. Med hjälp av kommandoradsargument kan du enkelt ändra beteendet hos ditt program utan att behöva ändra koden.

## Se även

Här är några användbara länkar för att läsa mer om hur man hanterar kommandoradsargument i TypeScript:

- [Node.js documentation on process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [CLI built-in options in Node.js](https://nodejs.org/api/cli.html#cli_builtin_options)
- [A Beginner's Guide to the Node.js CLI](https://www.digitalocean.com/community/tutorials/nodejs-command-line-arguments-nodejs-pt)
- [Parsing Command-Line Arguments in Node.js with Yargs](https://www.digitalocean.com/community/tutorials/how-to-use-command-line-arguments-in-node-js-using-yargs)

Jag hoppas att detta blogginlägg har varit informativt och att du nu känner dig mer bekväm med