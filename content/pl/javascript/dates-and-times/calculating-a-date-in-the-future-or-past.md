---
date: 2024-01-20 17:31:28.786415-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to\
  \ proces ustalania daty, kt\xF3ra b\u0119dzie mia\u0142a miejsce za okre\u015Blon\u0105\
  \ liczb\u0119 dni, miesi\u0119cy, czy lat od\u2026"
lastmod: '2024-03-13T22:44:35.809807-06:00'
model: gpt-4-1106-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to proces\
  \ ustalania daty, kt\xF3ra b\u0119dzie mia\u0142a miejsce za okre\u015Blon\u0105\
  \ liczb\u0119 dni, miesi\u0119cy, czy lat od\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to proces ustalania daty, która będzie miała miejsce za określoną liczbę dni, miesięcy, czy lat od konkretnej daty wyjściowej. Programiści robią to często, aby obsługiwać zadania związane z harmonogramami, przypomnieniami czy terminami.

## Jak to zrobić:
Załóżmy, że chcesz dodać 10 dni do dzisiejszej daty:

```javascript
let today = new Date();
let tenDaysLater = new Date(today.setDate(today.getDate() + 10));
console.log(tenDaysLater.toDateString());
```

Lub odejmij 5 lat:

```javascript
let fiveYearsEarlier = new Date(new Date().setFullYear(new Date().getFullYear() - 5));
console.log(fiveYearsEarlier.toDateString());
```

Wynik:
```
"Fri Apr 21 2023" // dla pierwszego kodu, zakładając, że dziś jest 11 kwietnia 2023.
"Sun Apr 11 2018" // dla drugiego kodu.
```

## Deep Dive:
Daty w JavaScript opierają się na obiekcie `Date`, który był częścią języka niemal od początku, odnosząc się do standardu ECMAScript 1 z 1997 roku. Obiekt `Date` może być niestabilny przez strefy czasowe i zmiany czasu letniego/zimowego. Alternatywą może być używanie bibliotek takich jak Moment.js, które ułatwiają pracę z datami. Kiedy dodajesz lub odejmujesz czas, pamiętaj, że nie wszystkie miesiące mają tyle samo dni i uwzględnij zmiany czasu letniego/zimowego.

## Zobacz także:
- MDN Web Docs - Date: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/ – nowoczesna biblioteka do pracy z datami.
