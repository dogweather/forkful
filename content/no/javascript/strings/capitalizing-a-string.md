---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:46.098714-07:00
description: "\xC5 kapitalisere en streng betyr \xE5 konvertere den f\xF8rste bokstaven\
  \ i strengen til stor bokstav mens de gjenv\xE6rende bokstavene forblir som de er.\
  \ Denne\u2026"
lastmod: 2024-02-19 22:05:00.443044
model: gpt-4-0125-preview
summary: "\xC5 kapitalisere en streng betyr \xE5 konvertere den f\xF8rste bokstaven\
  \ i strengen til stor bokstav mens de gjenv\xE6rende bokstavene forblir som de er.\
  \ Denne\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å konvertere den første bokstaven i strengen til stor bokstav mens de gjenværende bokstavene forblir som de er. Denne operasjonen utføres ofte i JavaScript for formatering av brukerinndata, visning av navn eller titler, og for å sikre konsistens i brukergrensesnittets tekster.

## Hvordan:
I JavaScript finnes det ikke en innebygd metode for direkte å kapitalisere strenger, men det er enkelt å implementere ved bruk av grunnleggende strengmanipuleringsmetoder.

### Ved bruk av standard JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Utdata: "Hello world"
```

### ES6-versjon
Med ES6-maltegnstrenge kan funksjonen skrives på en mer kortfattet måte:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Utdata: "Hello ES6"
```

### Ved bruk av Lodash
Lodash er et populært tredjeparts nyttebibliotek som tilbyr et bredt spekter av funksjoner for å manipulere og arbeide med JavaScript-verdier, inkludert strenger. For å kapitalisere en streng ved hjelp av Lodash:
```javascript
// Installer først lodash hvis du ikke allerede har: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH eksempel')); // Utdata: "Lodash eksempel"
```
_Merk hvordan Lodash ikke bare kapitaliserer den første bokstaven, men også konverterer resten av strengen til små bokstaver, noe som avviker noe fra den enkle JavaScript-implementeringen._

### Ved bruk av CSS (Kun for visningsformål)
Hvis målet er å kapitalisere tekst for visning i UI, kan CSS brukes:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Viser som "Hello css" -->
```
**Merk:** Denne metoden endrer hvordan teksten ser ut på nettsiden uten å endre selve strengen i JavaScript.
