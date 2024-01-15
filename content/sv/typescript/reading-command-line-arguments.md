---
title:                "Att läsa in kommandoradsargument"
html_title:           "TypeScript: Att läsa in kommandoradsargument"
simple_title:         "Att läsa in kommandoradsargument"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##
Vad är kommandoradsargument och varför bör du läsa om det?

Kommandoradsargument används för att tillhandahålla input till ditt program från terminalen. Genom att lära dig att läsa och använda kommandoradsargument i dina TypeScript-program, kan du öka programmets flexibilitet och effektivitet.

## Hur man läser kommandoradsargument i TypeScript

För att läsa kommandoradsargument i ett TypeScript-program, kan du använda process.argv-objektet.
Först måste vi importera process-modulen i vårt program:
```typescript
import * as process from 'process';
```
Sedan kan vi läsa kommandoradsargumenten genom att använda process.argv-objektet:
```typescript
const args = process.argv.slice(2);
```
process.argv-objektet returnerar en lista av kommandoradsargument som gavs till programmet. Genom att använda slice-metoden med ett argument på 2, kan vi undvika de första två argumenten som alltid är sökvägen till Node och sökvägen till vårt program.
Till exempel, för att läsa det första kommandoradsargumentet som gavs till programmet, kan vi använda:
```typescript
const firstArg = args[0];
```
Om inga kommandoradsargument ges när programmet körs, kommer args-objektet att vara en tom lista.

## Djupdykning i kommandoradsargument

I TypeScript kan du använda en rad olika metoder för att läsa och hantera kommandoradsargument.
En av de vanligaste metoderna är att använda ett externt bibliotek som commander eller yargs.
Dessa bibliotek ger en mer strukturerad och användarvänlig lösning för att hantera kommandoradsargument i dina program.
För en djupare förståelse av hur process.argv fungerar och hur du kan anpassa läsningen av kommandoradsargument i dina TypeScript-program, rekommenderar vi att du går igenom dokumentationen för process-modulen och utforskar olika bibliotek som erbjuder detta stöd.

## Se även

- [process-modulen dokumentation](https://nodejs.org/api/process.html)
- [commander biblioteket](https://github.com/tj/commander.js/)
- [yargs biblioteket](https://github.com/yargs/yargs/)