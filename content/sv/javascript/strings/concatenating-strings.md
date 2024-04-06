---
date: 2024-01-20 17:34:55.633490-07:00
description: "How to: (Hur man g\xF6r:) ."
lastmod: '2024-04-05T21:53:39.622981-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## How to: (Hur man gör:)
```javascript
// Använd '+' för att konkatenera
let greeting = "Hej " + "världen!";
console.log(greeting); // Output: Hej världen!

// Template literals med backticks (``)
let planet = "världen";
let greet = `Hallå ${planet}!`;
console.log(greet); // Output: Hallå världen!
```

## Deep Dive (Djupdykning)
Från början använde JavaScript `'+'` för att konkatenera strängar. Det är enkelt, men kan bli rörigt med många variabler och långa strängar. Sedan ES6 (ECMAScript 2015), har "template literals" förbättrat situationen. Med `${}` kan du infoga variabler direkt i strängen och behålla läsbarheten. JavaScript hanterar konkatenering genom att skapa en ny sträng varje gång, vilket kan vara minneskrävande i stora applikationer. Alternativ inkluderar `Array.join()` eller `String.concat()`, men de används mer sällan.

## See Also (Se även)
- [MDN - Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [MDN - String.concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN - Array.join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
