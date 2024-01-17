---
title:                "Läsning av kommandoradsargument"
html_title:           "TypeScript: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av kommandoradsargument är en process där programmerare hämtar värden från användarens inmatning när ett program körs i terminalen. Detta gör det möjligt för användaren att anpassa programmet efter sina behov och skapar interaktivitet mellan användaren och programmet.

## Så här gör du:
TypeScript ger ett enkelt sätt att läsa kommandoradsargument genom att använda process.argv-objektet. Här är ett exempel på hur du kan läsa och skriva ut olika argument som har matats in i terminalen:

```
// skriv ut första argumentet
console.log(process.argv[2]);

// skriv ut andra argumentet
console.log(process.argv[3]);

// skriv ut alla argument som en array
console.log(process.argv.slice(2));
```

Om vi matar in "tsc program.ts Hello World" i terminalen kommer ovanstående kod att ge följande output:

```
program.ts
Hello
[program.ts, Hello, World]
```

## Djupdykning:
Kommandoradsargument har funnits sedan tidigt i datorns historia och används fortfarande idag för att ge interaktion med programsystem som inte har ett grafiskt användargränssnitt. Alternativ till att läsa kommandoradsargument är att använda en grafisk användargränssnitt som ger en mer visuell och intuitiv interaktion med användaren. När det kommer till implementationen är det viktigt att ha i åtanke att process.argv-objektet är en global variabel i Node.js miljön och kan därför enkelt användas för att läsa kommandoradsargument.

## Se även:
- [Node.js Dokumentation: Process](https://nodejs.org/api/process.html#process_process_argv)
- [Använda kommandoradsargument i TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#arbitrary-js-expressions-as-tag-parameters)