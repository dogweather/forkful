---
aliases:
- /no/javascript/rounding-numbers/
date: 2024-01-26 03:45:41.398731-07:00
description: "Avrunding er \xE5 klippe av st\xF8yen etter et visst punkt i et tall.\
  \ Programmerere avrunder for \xE5 kontrollere presisjon, h\xE5ndtere minne, eller\
  \ gj\xF8re output\u2026"
lastmod: 2024-02-18 23:08:54.304330
model: gpt-4-0125-preview
summary: "Avrunding er \xE5 klippe av st\xF8yen etter et visst punkt i et tall. Programmerere\
  \ avrunder for \xE5 kontrollere presisjon, h\xE5ndtere minne, eller gj\xF8re output\u2026"
title: Avrunding av tall
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Avrunding er å klippe av støyen etter et visst punkt i et tall. Programmerere avrunder for å kontrollere presisjon, håndtere minne, eller gjøre output brukervennlig—som å endre 2.998 til et rent 3.

## Hvordan:
Her er hvordan du avrunder tall i JavaScript ved hjelp av `Math.round()`, `Math.ceil()`, og `Math.floor()`:

```javascript
let originalNumber = 2.567;

let rundetNed = Math.floor(originalNumber); // 2
let rundetOpp = Math.ceil(originalNumber);    // 3
let avrundet = Math.round(originalNumber);     // 3 (siden .567 er mer enn .5)

console.log(rundetNed); // Skriver ut: 2
console.log(rundetOpp);   // Skriver ut: 3
console.log(avrundet);     // Skriver ut: 3
```

For å fikse til et bestemt antall desimaler, bruk `toFixed()`:

```javascript
let toDesimaler = originalNumber.toFixed(2); // "2.57" (returnerer en streng)

console.log(toDesimaler); // Skriver ut: "2.57"
```

Konverter strengen tilbake til et tall med et unært pluss eller `Number()`:

```javascript
let tallIgjen = +toDesimaler; // 2.57

console.log(tallIgjen); // Skriver ut: 2.57
```

## Dypdykk
Å avrunde tall er ikke nytt; det er like gammelt som tallene selv. I JavaScript bruker `Math.round()` "rund halv opp"-grenseregelen: hvis den fraksjonelle delen er 0.5, avrunder den til det nærmeste partall.

For mer kontroll kan `toFixed()` være din go-to, men husk, den returnerer en streng. Å konvertere tilbake til et tall kan være et ekstra skritt, men sikrer at du fortsetter å jobbe med numeriske typer.

Alternativer? Biblioteker som `lodash` tilbyr `_.round(number, [presisjon=0])` for mer nyansert kontroll. Eller, den nyere `Intl.NumberFormat` gir deg høy-presisjonsformatering utover bare avrunding.

Når vi snakker om presisjon, vær oppmerksom på flyttalls særegenheter i JavaScript. `0.1 + 0.2` blir ikke nøyaktig `0.3` på grunn av hvordan tall lagres. Noen ganger blir avrunding nødvendig for å korrigere slike flyttallfeil.

## Se også
- Mozillas matematikkdokumentasjon: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Finansiell avrunding med `Intl.NumberFormat`: [ECMAScript Internationaliserings-API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` avrunding: [Lodash Dokumenter](https://lodash.com/docs/4.17.15#round)
