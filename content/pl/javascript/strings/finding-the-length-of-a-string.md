---
date: 2024-01-20 17:47:51.342833-07:00
description: "How to: (Jak to zrobi\u0107?) D\u0142ugo\u015B\u0107 \u0142a\u0144cucha\
  \ znak\xF3w w JavaScript to w\u0142asno\u015B\u0107 `.length`, dost\u0119pna od\
  \ pocz\u0105tku istnienia j\u0119zyka. W przesz\u0142o\u015Bci alternatywne\u2026"
lastmod: '2024-04-05T22:50:50.127859-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) D\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3\
  w w JavaScript to w\u0142asno\u015B\u0107 `.length`, dost\u0119pna od pocz\u0105\
  tku istnienia j\u0119zyka."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## How to: (Jak to zrobić?)
```javascript
// Prosty przykład
let greeting = "Cześć, jak się masz?";
console.log(greeting.length); // 21

// Użycie w funkcji
function stringLength(str) {
  return str.length;
}

console.log(stringLength("Dzień dobry")); // 11
```

## Deep Dive (Dogłębna analiza)
Długość łańcucha znaków w JavaScript to własność `.length`, dostępna od początku istnienia języka. W przeszłości alternatywne sposoby obliczania długości stringa były rzadkością, gdyż `.length` okazało się być szybkie i niezawodne. Dziedziczenie tej właściwości od prototypu `String` gwarantuje, że jest ona dostępna dla każdego stringa. Należy pamiętać, że właściwość `.length` zwraca liczbę jednostek kodu UTF-16 w łańcuchu, a nie faktyczną liczbę znaków Unicode, co może prowadzić do nieporozumień przy pracy z emoji lub niektórymi znakami specjalnymi.

## See Also (Zobacz również)
- MDN Documentation on strings: [MDN - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Unicode and JavaScript strings: [Understanding JavaScript Strings as a Unicode](https://mathiasbynens.be/notes/javascript-unicode)
