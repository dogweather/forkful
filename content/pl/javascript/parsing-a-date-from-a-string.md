---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:37:11.144578-07:00
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing daty ze stringa to proces zamiany tekstu w obiekt daty. Programiści to robią, by móc operować datami: dodawać dni, porównywać, wyświetlać w róznych formatach.

## How to: (Jak to zrobić:)
```Javascript
const dateString = '2023-04-01'; // przykładowy string z datą
const parsedDate = new Date(dateString);

console.log(parsedDate); // Pokaże datę w formacie obiektu Date

// A teraz formatowanie daty na przykładzie
const options = { year: 'numeric', month: 'long', day: 'numeric' };
const formattedDate = parsedDate.toLocaleDateString('pl-PL', options);

console.log(formattedDate); // Pokaże "1 kwietnia 2023"
```

## Deep Dive (Dogłębna analiza)
Wcześniej JavaScript nie miał wbudowanego wsparcia dla parsingu dat, przez co programiści często sięgali po biblioteki jak Moment.js. Od ES5 można użyć `Date.parse()` albo konstruktora `new Date()`, które radzą sobie z ISO 8601. Pamiętajmy, że interpretacja stringów bez standardu może być różna w zależności od przeglądarki.

Alternatywy jak Luxon, date-fns czy Day.js oferują więcej opcji i lepszą strefę czasową. Implementacje w różnych środowiskach mogą różnić się obsługą brzegowych przypadków. Gdy robimy parsing dat, warto być ostrożnym z formatami i zawsze testować.

## See Also (Zobacz również)
- [MDN Web Docs Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [MDN Web Docs Date() constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
- [date-fns library](https://date-fns.org/)
- [Day.js library](https://day.js.org/)
- [Luxon documentation](https://moment.github.io/luxon/#/)
