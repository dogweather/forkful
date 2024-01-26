---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:53:52.649198-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Debugging er prosessen med å finne og fikse feil i koden. Vi printer debug output for å se hva som faktisk skjer når koden kjører, noe som hjelper oss å forstå og rette feil raskt.

## How to:
For å printe debug output i TypeScript, bruk `console.log()`, `console.error()`, eller `console.warn()`. Her er et par eksempler:

```TypeScript
function add(a: number, b: number): number {
    console.log(`Adderer ${a} og ${b}`);
    return a + b;
}

const result = add(2, 3);
console.log(`Resultatet er: ${result}`);
```

Sample output:
```
Adderer 2 og 3
Resultatet er: 5
```

Hvis det er en feil, kan vi bruke `console.error()` for å fremheve problemet:

```TypeScript
function divide(a: number, b: number): number {
    if (b === 0) {
        console.error("Kan ikke dele med 0");
        return NaN; // Returnerer “Not a Number”
    }
    return a / b;
}

const result = divide(10, 0);
```

Sample output:
```
Kan ikke dele med 0
```

## Deep Dive
Før `console.log()` og venner, fantes det ikke et standardisert system for debug output. Programmerere brukte forskjellige metoder, som å skrive ut til filer eller bruke dedikerte debugging-verktøy. 

Nå er `console`-objektet innebygd i de fleste JavaScript-motorer, inkludert Node.js og nettlesere, noe som gir en konsistent måte å loggføre data og feil. 

`console.log()` er bra for generell informasjon, mens `console.error()` og `console.warn()` er tenkt brukt for feil og advarsler, som kan hjelpe i produksjonsmiljøer for å skille mellom logg-nivåer.

Det finnes alternativer til `console`:

- Node.js har en `debug`-modul som gir mer kontroll over hva som skrives ut og når.
- Nettleserutvidelser og verktøy som debugger i Chrome DevTools gir en mer interaktiv opplevelse.
- Logging-biblioteker som `winston` og `log4js` gir flere funksjoner som tilpassede loggnivåer og loggformat.

Hver av disse metodene har sine styrker, så velg den som passer best for ditt prosjekt og din feilsøkingsstil.

## See Also
- MDN Web Docs om `console`: https://developer.mozilla.org/en-US/docs/Web/API/Console/log
- Node.js `debug` dokumentasjon: https://nodejs.org/api/debugger.html
- Winston logging-bibliotek: https://github.com/winstonjs/winston
- Log4js GitHub-side: https://github.com/log4js-node/log4js-node
