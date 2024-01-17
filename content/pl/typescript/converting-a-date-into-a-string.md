---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "TypeScript: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty na ciąg znaków oznacza przekształcenie danych zawierających informacje o dacie, np. 01/01/2020, w zapis tekstowy, np. stycznia pierwszego tysiąc dziewięćset dwudziestego roku. Programiści wykonują to przekształcenie, ponieważ jest to często potrzebne przy wyświetlaniu dat lub przetwarzaniu ich w różnych aplikacjach.

## Jak to zrobić:
W TypeScript konwersję daty na ciąg znaków można przeprowadzić za pomocą wbudowanej funkcji ```toString()```. Przykład użycia:
```
let date = new Date("2020/01/01");
console.log(date.toString());
// Output: Wed Jan 01 2020 00:00:00 GMT+0100 (czas środkowoeuropejski standardowy)
```
Można również użyć opcjonalnego parametru ```toLocaleDateString()``` do dostosowania formatu zwracanego ciągu znaków. Przykład:
```
let date = new Date("2020/01/01");
console.log(date.toLocaleDateString("pl-PL"));
// Output: 01.01.2020
```

## Głębsza analiza:
Konwersja daty na ciąg znaków jest ważna ze względu na różne sposoby wyświetlania dat w różnych kulturach i językach. Można również skorzystać z bibliotek takich jak Moment.js, aby łatwiej manipulować datami lub zastosować własną logikę przekształcania daty w ciąg znaków.

## Zobacz również:
- [Dokumentacja TypeScript do funkcji ```toString()```](https://www.typescriptlang.org/docs/handbook/date-and-time.html#tostring)
- [Biblioteka Moment.js](https://momentjs.com/)
- [Poradnik na temat konwersji daty w TypeScript](https://www.geeksforgeeks.org/how-to-convert-date-to-string-in-typescript/)