---
title:                "Javascript: Lesing av kommandolinje-argumenter"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter kan virke som en enkel og kanskje ubetydelig oppgave i JavaScript-programmering, men det er faktisk en viktig og nyttig ferdighet å ha. Å kunne lese kommandolinjeargumenter lar deg lage programmer som kan tilpasses og konfigureres av brukeren, og det kan også hjelpe deg med feilsøking og testing av kode. Det kan virke litt skremmende i begynnelsen, men å lære å lese kommandolinjeargumenter vil definitivt være nyttig for alle som vil bli bedre på JavaScript-programmering.

## Hvordan

Det første trinnet for å lese kommandolinjeargumenter i JavaScript er å definere en variabel som vi vil bruke til å lagre argumentene. Dette kan gjøres ved hjelp av `process.argv` objektet, som er en innebygd funksjon i Node.js for å få tilgang til kommandolinjeargumentene. La oss se på et eksempel:

```Javascript
// Første argument (index 0) er alltid selve stien til filen som blir kjørt
// Andre argument (index 1) er filnavnet
// Tredje argument (index 2) og utover er alle kommandolinjeargumentene
let arguments = process.argv.slice(2);

// Output: [ 'arg1', 'arg2', 'arg3' ]
console.log(arguments);
```

I dette eksempelet har vi brukt `slice()` metoden for å fjerne de to første argumentene (filnavnet og stien) og bare fått tilgang til selve kommandolinjeargumentene. Nå kan vi enkelt bruke disse argumentene i koden vår. La oss si at vi har et program som tar inn to tall og multipliserer dem:

```Javascript
let arguments = process.argv.slice(2);

// Sjekker om det er to argumenter (to tall som skal multipliseres)
if (arguments.length !== 2) {
  console.log("Du må angi to tall som argumenter");
} else {
  // Multipliserer de to tallene og outputter resultatet
  let result = arguments[0] * arguments[1];
  console.log(`${arguments[0]} * ${arguments[1]} = ${result}`);
}
```

La oss si at vi kjører programmet vårt fra kommandolinjen ved å skrive inn `node program.js 5 10`. Programmet vil da outputte `5 * 10 = 50` og multipliserer de to argumentene vi angav.

## Dypdykk

Selv om det første eksempelet vi så på er ganske enkelt, kan lesing av kommandolinjeargumenter bli mye mer komplekst og nyttig i mer omfattende programmer. Det er også verdt å merke seg at kommandolinjeargumentene ikke alltid er tall, de kan være tekststrenger, boolske verdier eller til og med filnavn og -baner. Det er derfor viktig å ha god oversikt over kommandolinjeargumentene og bruke relevante metoder, som `split()` og `join()`, for å formatere og bruke dem i koden din.

En annen viktig ting å huske på når du arbeider med kommandolinjeargumenter er å ta hensyn til brukerinput og sørge for at koden din håndterer feil og ugyldige argumenter på en god måte. Dette kan gjøres ved hjelp av try/catch blokker eller ved å bruke betingete uttrykk og å sjekke på forhånd om argumentene er gyldige.

## Se også

- [process.argv | Node.js v16.5.0 Documentation](https://nodejs.org/api/process.html#process_process_argv)
- [Command-line arguments - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/apply)