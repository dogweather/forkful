---
title:                "TypeScript: Att läsa kommandoradsargument"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en vanlig praxis inom programmering för att kunna få input från användaren. Genom att kunna hantera kommandoradsargument kan du skriva mer användarvänliga program som kan anpassas baserat på användarens behov och preferenser.

## Hur man gör det

För att läsa kommandoradsargument i TypeScript behöver du använda dig av inbyggda Node.js API:et "process". Genom att använda process.argv kan du komma åt en array med alla kommandoradsargument som användaren anger vid körning av programmet.

```TypeScript
// Exempelkod för att läsa in kommandoradsargument
const argOne = process.argv[2];
const argTwo = process.argv[3];

console.log(`Första argumentet är: ${argOne}`);
console.log(`Andra argumentet är: ${argTwo}`);
```

Om vi kör det här exemplet med följande kommandoradsargument:

```bash
node read-arguments.ts hello world
```

Så kommer vi få följande output:

```bash
Första argumentet är: hello
Andra argumentet är: world
```

Som du kan se så är process.argv en array där det första argumentet är node, det andra argumentet är filnamnet för vårt program och resterande argument är det som användaren anger.

## Djupdykning

Det finns även möjlighet att använda ett tredje argument för att läsa in kommandoradsargument på ett mer avancerat sätt. Genom att använda flaggor, eller prefixes, kan du ange förväntade argument för ditt program.

```TypeScript
// Exempelkod för att använda flaggor för kommandoradsargument
const args = process.argv.slice(2);

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--input") {
    console.log(`Förväntat argument för input: ${args[i + 1]}`);
  }
  if (args[i] === "--output") {
    console.log(`Förväntat argument för output: ${args[i + 1]}`);
  }
}
```

Om vi kör det här exemplet med följande kommandoradsargument:

```bash
node read-arguments.ts --input input.txt --output output.txt
```

Så kommer vi få följande output:

```bash
Förväntat argument för input: input.txt
Förväntat argument för output: output.txt
```

Genom att använda flaggor kan vi enkelt specificera förväntade argument för olika delar av vårt program.

## Se även

- Node.js process documentation: https://nodejs.org/api/process.html#process_process_argv
- Läsa kommandoradsargument i Node.js: https://www.digitalocean.com/community/tutorials/nodejs-command-line-arguments-sv