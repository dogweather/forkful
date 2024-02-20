---
date: 2024-01-20 17:35:06.629855-07:00
description: "\xC5 sammenk\xE6de strenger (strings) betyr \xE5 sette dem sammen i\
  \ \xE9n. Vi gj\xF8r det for \xE5 bygge tekstbasert data, som meldinger eller kode,\
  \ p\xE5 en dynamisk og\u2026"
lastmod: 2024-02-19 22:05:00.452083
model: gpt-4-1106-preview
summary: "\xC5 sammenk\xE6de strenger (strings) betyr \xE5 sette dem sammen i \xE9\
  n. Vi gj\xF8r det for \xE5 bygge tekstbasert data, som meldinger eller kode, p\xE5\
  \ en dynamisk og\u2026"
title: "Sammensl\xE5ing av strenger"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å sammenkæde strenger (strings) betyr å sette dem sammen i én. Vi gjør det for å bygge tekstbasert data, som meldinger eller kode, på en dynamisk og fleksibel måte.

## How to: (Hvordan:)
```javascript
// Bruker plusstegn (+) for å sammenkæde strenger
let hilsen = 'Hei, ' + 'verden!';
console.log(hilsen); // Output: "Hei, verden!"

// Bruker template literals med backticks (``) og ${}
let navn = 'Norge';
let velkomst = `Velkommen til ${navn}!`;
console.log(velkomst); // Output: "Velkommen til Norge!"
```

## Deep Dive (Dypdykk)
Før i tiden, da JavaScript var ungt, var pluss-operatoren (+) det vanligste verktøyet for å sammenflette strenger. I ES5 (ECMAScript 5), ble dette standarden - enkelt, men problematisk med mange variabler og kompleks struktur. 

Kom ES6 (ECMAScript 2015), og med det kom template literals. De gjør det lettere å inneholde variabler og uttrykk, samt å håndtere større tekstblokker og interpolasjon.

Man kan også bruke `concat()` metoden, men det er mindre vanlig:
```javascript
let del1 = 'God';
let del2 = 'dag';
let fullSetning = del1.concat(' ', del2);
console.log(fullSetning); // Output: "God dag"
```
Performans: I store løkker er direkte konkatinering (+) kjent for å være mindre effektiv enn å bruke array `join()` metoden, grunnet måten JavaScript håndterer minnet for strenger. 

## See Also (Se Også)
- MDN Web Docs for Strings: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
- JavaScript Template Literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- JavaScript Performance and String Concatenation: https://jsperf.com/plus-vs-concat
