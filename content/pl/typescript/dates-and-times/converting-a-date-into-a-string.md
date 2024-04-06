---
date: 2024-01-20 17:37:36.027522-07:00
description: "How to: (Jak to zrobi\u0107:) Wczesne systemy komputerowe, przez ograniczenia\
  \ pami\u0119ci i moc obliczeniow\u0105, u\u017Cywa\u0142y r\xF3\u017Cnych sposob\xF3\
  w reprezentacji czasu \u2013\u2026"
lastmod: '2024-04-05T22:50:49.454858-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wczesne systemy komputerowe, przez ograniczenia pami\u0119\
  ci i moc obliczeniow\u0105, u\u017Cywa\u0142y r\xF3\u017Cnych sposob\xF3w reprezentacji\
  \ czasu \u2013 najcz\u0119\u015Bciej jako liczby ca\u0142kowite."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to: (Jak to zrobić:)
```TypeScript
const currentDate: Date = new Date();
// Zamiana na lokalny format daty i czasu
const localDateString: string = currentDate.toLocaleString("pl-PL");
console.log(localDateString); // 'dd.mm.yyyy, HH:MM:SS'

// Zamiana na format ISO
const isoDateString: string = currentDate.toISOString();
console.log(isoDateString); // 'yyyy-mm-ddTHH:MM:SS.mmmZ'

// Wykorzystanie metody toString()
const simpleDateString: string = currentDate.toString();
console.log(simpleDateString); // 'Wed Apr 05 2023 14:56:17 GMT+0200 (czas środkowoeuropejski letni)'
```

## Deep Dive (Dogłębna analiza)
Wczesne systemy komputerowe, przez ograniczenia pamięci i moc obliczeniową, używały różnych sposobów reprezentacji czasu – najczęściej jako liczby całkowite. Metody konwersji dat na string w TypeScript (dziedziczące po JavaScript) wynikają stąd, ale też z późniejszej potrzeby obsługi formatów zrozumiałych dla ludzi. Alternatywy obejmują ręczną konstrukcję ciągów znaków z elementów daty, lub użycie bibliotek zewnętrznych jak moment.js (obecnie często zastępowane przez nowsze rozwiązania jak date-fns lub luxon).

Implementacja `Date.prototype.toString()` daje czytelny, ale maszynowo różnie interpretowany ciąg znaków. `Date.prototype.toISOString()` zwraca datę w standardzie ISO 8601, poprawnie odbieranym przez różne systemy i przeglądarki. Wykorzystanie `toLocaleString()` pozwala przedstawić datę w formacie lokalnym, idealnym do wyświetlenia end-userom. Ważne, by pamiętać o strefach czasowych podczas pracy z datami w aplikacjach międzynarodowych.

## See Also (Zobacz również)
- MDN Web Docs Date reference: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Luxon documentation for modern date handling: [Luxon](https://moment.github.io/luxon/#/)
- Date-fns library as a modern alternative: [date-fns](https://date-fns.org/)
- About ISO 8601 standard: [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
