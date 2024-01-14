---
title:    "TypeScript: Utskrift av feilsøkingsutskrift"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive kode kan være en utfordrende prosess, og det er ikke uvanlig å støte på feil underveis. En enkel og effektiv måte å finne og fikse disse feilene er å benytte seg av debugging og debug-utskrifter. Ved å legge inn print-utskrifter i koden, kan du se nøyaktig hvor programmet ditt "bryter sammen" og finne en løsning. Derfor er å inkludere debug-utskrifter i koden et viktig verktøy for å effektivisere utviklingsprosessen og forbedre kvaliteten på koden din.

## Hvordan

For å inkludere debug-utskrifter i koden din i TypeScript, kan du bruke funksjonen `console.log()`. Dette er en innebygd funksjon som skriver ut en melding til konsollen.

```TypeScript
console.log("Hei, dette er en debug-utskrift");
```

Du kan også inkludere variabler i debug-utskriften ved å bruke såkalte templetsttrenger, som erstattes med verdien av variabelen når koden kjører.

```TypeScript
let navn = "Olav";
console.log(`Hei, mitt navn er ${navn}`);
```

Når koden din kjører, vil du se disse meldingene i konsollen i utviklerverktøyet ditt, og du kan bruke dem til å finne og fikse eventuelle feil.

## Dykk dypere

Det er også mulig å legge til ytterligere informasjon i debug-utskrifter ved å bruke `console.debug()`, `console.warn()` og `console.error()` som gir ulike typer utskrifter. Du kan også bruke objekter og arrays i debug-utskrifter og formatere utskriften med spesielle tegn.

Se dokumentasjonen for `console` i TypeScript for å lære mer om mulighetene for debug-utskrifter.

## Se også
- [Dokumentasjon for `console` i TypeScript](https://www.typescriptlang.org/docs/handbook/console.html)
- [En gjennomgang av debugging i TypeScript](https://dzone.com/articles/the-art-of-debugging-typescript)
- [5 tips for effektiv debugging i TypeScript](https://codingsans.com/blog/debugging-typescript)