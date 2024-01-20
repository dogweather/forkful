---
title:                "Skriva till standardfel"
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (`stderr`) involve att skicka felmeddelanden och andra diagnoser separat från huvudprogrammets utdata. Programmerare gör detta för att hantera fel och loggning så att dessa inte blandas ihop med den normala utdatan (`stdout`).

## Hur gör man:

```typescript
// Skriv till standardfel i Node.js
process.stderr.write('Error: Ett fel inträffade!\n');

// Använda console.error för att skriva till standardfel
console.error('Error: Ett annat fel inträffade!');
```

Exempel utskrift:

```
Error: Ett fel inträffade!
Error: Ett annat fel inträffade!
```

## Djupdykning:

Standardfel (`stderr`) är en av de tre standardströmmar som introducerades i Unix och har sedan blivit standard i andra operativsystem. Alternativ till `stderr` inkluderar att skriva till en loggfil eller att använda externa loggningstjänster. I Node.js implementeras `stderr` som en skrivbar ström, vilket betyder att du kan använda alla metoder för skrivbara strömmar, som `write()` eller piping (`pipe()`).

## Se även:

- Node.js documentation on process.stderr: https://nodejs.org/api/process.html#process_process_stderr
- Console.error() documentation at MDN: https://developer.mozilla.org/docs/Web/API/Console/error
- Understanding standard streams (stdin, stdout, stderr): https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html