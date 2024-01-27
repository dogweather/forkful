---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Regular expressions, eller regex, är verktyg för att matcha textmönster. Programmerare använder det för att söka, validera eller ersätta text, vilket ger smidig och effektiv hantering av strängdata.

## Hur gör man:
```javascript
// Söka efter alla förekomster av 'katt' i en sträng
const text = 'Katten rullade blixtsnabbt. Katten är snabb och smidig.';
const regex = /katt/gi;
const found = text.match(regex);
console.log(found); // Utmatning: ['Katten', 'Katten']

// Validera ett e-postformat
const email = 'någon@exempel.se';
const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
console.log(emailRegex.test(email)); // Utmatning: true

// Ersätta ordet 'katt' med 'tiger'
const newText = text.replace(/katt/gi, 'tiger');
console.log(newText); // Utmatning: 'Tigern rullade blixtsnabbt. Tigern är snabb och smidig.'
```

## Djupdykning:
Regular expressions härstammar från teoretisk datavetenskap och formaliserades på 1950-talet. De är integrerade i de flesta programmeringsspråk, inte bara JavaScript. Alternativ till regex inkluderar inbyggda strängmetoder som `indexOf` eller `includes`, men dessa saknar regexens kraft och finjustering. När du implementerar regex är prestanda en övervägning; komplexa uttryck kan vara resurskrävande.

## Se även:
- MDN Web Docs för RegExp: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp
- JavaScript.info's guide till Regular Expressions: https://javascript.info/regular-expressions
- RegExr för att testa dina regex online: https://regexr.com/
