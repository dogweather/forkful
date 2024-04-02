---
date: 2024-01-20 17:47:51.342833-07:00
description: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w to po prostu\
  \ okre\u015Blenie, ile znak\xF3w znajduje si\u0119 w tek\u015Bcie. Programi\u015B\
  ci robi\u0105 to, aby walidowa\u0107 dane wej\u015Bciowe,\u2026"
lastmod: '2024-03-13T22:44:35.787494-06:00'
model: gpt-4-1106-preview
summary: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w to po prostu\
  \ okre\u015Blenie, ile znak\xF3w znajduje si\u0119 w tek\u015Bcie. Programi\u015B\
  ci robi\u0105 to, aby walidowa\u0107 dane wej\u015Bciowe,\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## What & Why? (Co i Dlaczego?)
Znalezienie długości łańcucha znaków to po prostu określenie, ile znaków znajduje się w tekście. Programiści robią to, aby walidować dane wejściowe, przetwarzać tekst czy zarządzać pamięcią.

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
