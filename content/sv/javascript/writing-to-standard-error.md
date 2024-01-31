---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
`Standard error` (stderr) är en output-kanal för att visa felmeddelanden och loggar. Programmerare använder den för att skilja vanlig data från fel, vilket är användbart för felsökning och loggning.

## How to:
Skriv till stderr med `console.error()`.

```javascript
console.error('Det här är ett felmeddelande!');
```

Output i terminal:
```
Det här är ett felmeddelande!
```

Använd `process.stderr.write()` för att skriva utan newline-tecken.

```javascript
process.stderr.write('Felmeddelande utan newline');
```

Output i terminal:
```
Felmeddelande utan newline%
```

## Deep Dive
Stderr härstammar från Unix och C systemanrop. Det är filnummer 2, medan standard output (stdout) är 1. Alternativ till stderr inkluderar loggfiler och externa loggtjänster. Implementation av stderr i Node.js används genom `process`-objektet som en Writable Stream.

## See Also
- Node.js documentation on console.error() (https://nodejs.org/api/console.html#console_console_error_data_args)
- Node.js process.stderr (https://nodejs.org/api/process.html#process_process_stderr)
- Stream handling in Node.js (https://nodejs.org/api/stream.html)
