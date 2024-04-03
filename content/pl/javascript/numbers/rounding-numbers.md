---
date: 2024-01-26 03:45:32.564069-07:00
description: "Zaokr\u0105glanie polega na odci\u0119ciu nadmiaru po okre\u015Blonym\
  \ miejscu w liczbie. Programi\u015Bci zaokr\u0105glaj\u0105 w celu kontrolowania\
  \ dok\u0142adno\u015Bci, zarz\u0105dzania pami\u0119ci\u0105\u2026"
lastmod: '2024-03-13T22:44:35.791407-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie polega na odci\u0119ciu nadmiaru po okre\u015Blonym miejscu\
  \ w liczbie."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Co i dlaczego?
Zaokrąglanie polega na odcięciu nadmiaru po określonym miejscu w liczbie. Programiści zaokrąglają w celu kontrolowania dokładności, zarządzania pamięcią lub uczynienia wyjścia przyjaznym dla użytkownika - jak zmienienie 2.998 w czyste 3.

## Jak to zrobić:
Oto jak zaokrąglasz liczby w JavaScript za pomocą `Math.round()`, `Math.ceil()` oraz `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (ponieważ .567 to więcej niż .5)

console.log(roundedDown); // Wyświetla: 2
console.log(roundedUp);   // Wyświetla: 3
console.log(rounded);     // Wyświetla: 3
```

Aby ustawić określoną liczbę miejsc po przecinku, użyj `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (zwraca ciąg znaków)

console.log(twoDecimals); // Wyświetla: "2.57"
```

Konwertuj ciąg znaków z powrotem na liczbę za pomocą plusa jednostkowego lub `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Wyświetla: 2.57
```

## Głębsze spojrzenie
Zaokrąglanie liczb nie jest niczym nowym; jest tak stare jak same liczby. W JavaScript, `Math.round()` używa zasady "zaokrągl w górę do połowy": jeśli część ułamkowa wynosi 0.5, zaokrągla do najbliższej parzystej liczby.

Dla większej kontroli, `toFixed()` może być twoim pierwszym wyborem, ale pamiętaj, że zwraca ciąg znaków. Konwersja z powrotem na liczbę może być dodatkowym krokiem, ale zapewnia, że nadal pracujesz z typami numerycznymi.

Alternatywy? Biblioteki takie jak `lodash` oferują `_.round(number, [precision=0])` dla bardziej subtelnej kontroli. Lub, nowszy `Intl.NumberFormat` daje ci wysoką precyzję formatowania poza samym zaokrąglaniem.

Mówiąc o precyzji, uważaj na dziwactwa związane z liczbami zmiennoprzecinkowymi w JavaScript. `0.1 + 0.2` nie równa się dokładnie `0.3` ze względu na sposób przechowywania liczb. Czasami, zaokrąglanie staje się konieczne, aby skorygować takie błędy zmiennoprzecinkowe.

## Zobacz również
- Dokumentacja Math Mozilli: [MDN Web Docs](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Zaokrąglanie finansowe z `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- Zaokrąglanie `lodash`: [Dokumentacja Lodash](https://lodash.com/docs/4.17.15#round)
