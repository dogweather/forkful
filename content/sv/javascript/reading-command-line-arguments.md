---
title:                "Javascript: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradsargument är en viktig del av Javascript-programmering eftersom det ger dig möjlighet att interagera med användaren på ett dynamiskt sätt. Genom att kunna läsa in matade indata via kommandoraden kan du skapa program som är mer flexibla och anpassningsbara.

## Hur man gör
Att läsa kommandoradsargument i Javascript är ganska enkelt. Använd funktionen `process.argv` för att hämta en array med alla de argument som matats in från kommandoraden. Du kan sedan loopa igenom arrayen för att få tillgång till varje specifikt argument. Se till att ange argumenten separerade med mellanslag när du kör ditt program.

```Javascript
// Exempelkod för att läsa kommandoradsargument
let arguments = process.argv;

// Loopar igenom argumenten och skriver ut dem
for (let i = 0; i < arguments.length; i++) {
  console.log(`Argument ${i+1}: ${arguments[i]}`);
}
```

Om du kör ovanstående kod och matar in "node index.js hello world" från kommandoraden, kommer konsolen att skriva ut:

```bash
Argument 1: /usr/local/bin/node
Argument 2: /path/till/ditt/program/index.js
Argument 3: hello
Argument 4: world
```

## Djupdykning
Det finns ett par viktiga saker att hålla i minnet när man läser kommandoradsargument i Javascript. För det första kommer `process.argv` alltid att inkludera de två argument som visas i exemplet ovan, även om du inte matat in några argument från kommandoraden. Det första argumentet är sökvägen till noden och det andra är sökvägen till ditt program. Om du vill exkludera dessa och enbart få åtkomst till de argument som användaren matar in, kan du enkelt använda `process.argv.slice(2)`.

För det andra kommer alla argument som du matar in från kommandoraden att betraktas som strängar. Om du vill använda argumenten som nummer eller booleska värden, måste du konvertera dem till rätt datatyp.

## Se även
- [Node.js process.argv dokumentation](https://nodejs.org/api/process.html#process_process_argv)
- [W3Schools - Node.js process.argv](https://www.w3schools.com/nodejs/ref_process.asp)
- [FreeCodeCamp - Node.js process.argv tutorial](https://www.freecodecamp.org/news/how-to-use-process-argv-in-node-js/)