---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Javascript: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive ut feilsøkingsinformasjon, også kjent som debug output, er en viktig praksis for å finne og fikse feil i din JavaScript-kode. Det hjelper deg med å forstå hvordan koden din fungerer, og kan gi verdifull informasjon for å løse problemer i ditt program.

## Hvordan
For å skrive ut debug output i JavaScript, kan du bruke funksjonen `console.log()`. Denne funksjonen lar deg skrive ut en verdi eller et objekt til konsollen, som er en del av utviklerverktøyene i nettleseren din.

```Javascript
// Skriver ut en enkel tekststreng
console.log("Hei, verden!");

// Skriver ut en verdi fra en variabel
const navn = "Sara";
console.log("Hei, mitt navn er " + navn);

// Skriver ut et objekt 
const bruker = { navn: "John", alder: 27, yrke: "utvikler" };
console.log(bruker);
```

Output i konsollen:
```
Hei, verden!
Hei, mitt navn er Sara
{ navn: 'John', alder: 27, yrke: 'utvikler' }
```

## Deep Dive
I tillegg til `console.log()`, finnes det andre nyttige funksjoner for å skrive ut debug output i JavaScript. Noen av dem er `console.error()` for å skrive ut feilmeldinger, `console.warn()` for å advare om potensielle problemer og `console.info()` for å gi informasjon til utviklere.

Det finnes også andre måter å bruke `console`-objektet på, som å skrive ut en tabell med data ved hjelp av `console.table()` eller å måle tiden det tar for et stykke kode å utføre ved hjelp av `console.time()` og `console.timeEnd()`.

## Se også
- [Mozilla Developer Network - Debugging JavaScript](https://developer.mozilla.org/nb/docs/Web/JavaScript/Debugging)
- [W3Schools - JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp)
- [Chrome DevTools Documentation - Console API Reference](https://developers.google.com/web/tools/chrome-devtools/console/api)