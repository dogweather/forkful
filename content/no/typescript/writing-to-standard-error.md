---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) er en output-strøm for å skrive feilmeldinger og logge. Det gjør at vi kan skille vanlig output fra feil og diagnostisk informasjon.

## How to:
For å skrive til stderr i TypeScript, bruk `process.stderr.write` for strenger eller `console.error` for mer komplekse meldinger.

```TypeScript
// Skriver en enkel feilmelding til stderr
process.stderr.write('En feil har oppstått!\n');

// Bruker console.error for å skrive en feilmelding med variabler
const errorObj = { id: 7, message: 'Ugyldig operasjon' };
console.error('Detaljert feil:', errorObj);

// Eksempel output til terminalen
// En feil har oppstått!
// Detaljert feil: { id: 7, message: 'Ugyldig operasjon' }
```

## Deep Dive
Standard output (stdout) og standard error (stderr) stammer fra Unix og historiske operativsystemer, hvor de skapte skille mellom normal data og feildata. Når du skriver feilmeldinger til stderr, kan brukere omdirigere disse separat fra standard output. I node.js, som TypeScript ofte kjører på, er `process.stderr` en skrivbar strøm, mens `console.error` også skriver til stderr, men håndterer flere datatyper og gir formattering.

## See Also
- Node.js dokumentasjon på `console.error`: [Node.js console.error](https://nodejs.org/api/console.html#consoleerrordata-args)
- Node.js dokumentasjon på `process.stderr`: [Node.js process.stderr](https://nodejs.org/api/process.html#processstderr)
- Guide til strømmer (streams) i node.js: [Stream Handbook](https://nodesource.com/blog/understanding-streams-in-nodejs/)
