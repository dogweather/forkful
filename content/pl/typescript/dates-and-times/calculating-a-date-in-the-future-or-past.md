---
date: 2024-01-20 17:32:01.932467-07:00
description: "Co to jest obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142\
  o\u015Bci? To po prostu spos\xF3b na ustalenie, jaka data b\u0119dzie za kilka dni,\
  \ miesi\u0119cy czy lat, lub jaka\u2026"
lastmod: '2024-03-13T22:44:35.153053-06:00'
model: gpt-4-1106-preview
summary: "Co to jest obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015B\
  ci? To po prostu spos\xF3b na ustalenie, jaka data b\u0119dzie za kilka dni, miesi\u0119\
  cy czy lat, lub jaka\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
---

{{< edit_this_page >}}

## What & Why?
Co to jest obliczanie daty w przyszłości lub przeszłości? To po prostu sposób na ustalenie, jaka data będzie za kilka dni, miesięcy czy lat, lub jaka była. Programiści to robią, by obsługiwać rezerwacje, harmonogramy, przypomnienia – cokolwiek, co związane jest z czasem.

## How to:
```TypeScript
const calculateDate = (startingDate: Date, daysDelta: number): Date => {
  const resultDate = new Date(startingDate);
  resultDate.setDate(resultDate.getDate() + daysDelta);
  return resultDate;
};

const today = new Date();
const nextWeek = calculateDate(today, 7);
const lastMonth = calculateDate(today, -30);

console.log(`Next week: ${nextWeek.toLocaleDateString()}`);
console.log(`Last month: ${lastMonth.toLocaleDateString()}`);
```
Output:
```
Next week: 04/14/2023
Last month: 03/11/2023
```

## Deep Dive
Data i czas to fundamentalne pojęcia, z którymi programiści muszą się zmagać od początku ery komputerów. JavaScript (a więc i TypeScript) używają obiektu `Date` do manipulacji datami. Można dodawać lub odejmować dni używając metody `setDate` i `getDate`.

Istnieją alternatywy takie jak biblioteki `moment.js` czy `date-fns`, które oferują bardziej zaawansowane operacje i formatowanie dat, ale mogą być nadmiarem dla prostych operacji. TypeScript pozwala na strukturę i typowanie, co czyni operacje na datach bardziej przewidywalne.

Kolejnym ważnym kwestią jest strefa czasowa. Praca na obiektach `Date` w JS bez uwzględniania stref czasowych może prowadzić do nieoczekiwanych wyników. Domyślnie `Date` używa strefy czasowej ustawionej w systemie użytkownika, co trzeba mieć na uwadze podczas operacji na datach.

## See Also
- MDN Web Docs for Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- date-fns: https://date-fns.org/
- moment.js: https://momentjs.com/
- Luxon: https://moment.github.io/luxon/#/
