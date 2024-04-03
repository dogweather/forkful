---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:44.571840-07:00
description: "Hur g\xF6r man: I JavaScript finns ingen inbyggd metod f\xF6r att direkt\
  \ g\xF6ra f\xF6rsta bokstaven i en str\xE4ng stor, men det \xE4r rakt fram att implementera\
  \ med\u2026"
lastmod: '2024-03-13T22:44:38.276684-06:00'
model: gpt-4-0125-preview
summary: "I JavaScript finns ingen inbyggd metod f\xF6r att direkt g\xF6ra f\xF6rsta\
  \ bokstaven i en str\xE4ng stor, men det \xE4r rakt fram att implementera med grundl\xE4\
  ggande str\xE4ngmanipuleringsmetoder."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur gör man:
I JavaScript finns ingen inbyggd metod för att direkt göra första bokstaven i en sträng stor, men det är rakt fram att implementera med grundläggande strängmanipuleringsmetoder.

### Använda Standard JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Utdata: "Hello world"
```

### ES6 Version
Med ES6 mallsträngar kan funktionen skrivas på ett mer koncist sätt:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Utdata: "Hello ES6"
```

### Använda Lodash
Lodash är ett populärt tredjepartsverktygsbibliotek som erbjuder ett brett utbud av funktioner för att manipulera och arbeta med JavaScript-värden, inklusive strängar. För att göra första bokstaven i en sträng stor med Lodash:
```javascript
// Först, installera lodash om du inte har gjort det: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Utdata: "Lodash example"
```
_Notera hur Lodash inte bara gör första bokstaven stor utan också konverterar resten av strängen till små bokstäver, vilket skiljer sig lite från den rena JavaScript-implementeringen._

### Använda CSS (Endast för visningsändamål)
Om målet är att göra text stor för visning i användargränssnittet, kan CSS användas:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Visas som "Hello css" -->
```
**Obs:** Denna metod ändrar hur texten ser ut på webbsidan utan att ändra själva strängen i JavaScript.
