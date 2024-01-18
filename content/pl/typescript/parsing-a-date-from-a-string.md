---
title:                "Analizowanie daty z ciągu znaków"
html_title:           "TypeScript: Analizowanie daty z ciągu znaków"
simple_title:         "Analizowanie daty z ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Parsowanie daty z ciągu znaków to proces konwertowania daty zapisanej w formacie tekstowym na format możliwy do używania przez programy komputerowe. Programiści często parsują daty z ciągu znaków, aby móc je łatwo manipulować i wykorzystywać w swoich aplikacjach.

## Jak to zrobić:
Przykładowy kod i wyjście w języku TypeScript znajdują się w poniższych blokach kodu:
```TypeScript
const date = new Date('2021-01-01');
console.log(date.getDate()); // 1
console.log(date.getMonth()); // 0  (January starts at 0 in Date objects)
console.log(date.getFullYear()); // 2021
```

## Głębszy zanurzenie:
Parsowanie dat z ciągu znaków to problem, który pojawił się już w początkach programowania. Wcześniej, gdy istniał tylko jeden format daty, taki jak DD/MM/YYYY, programiści nie musieli parsować dat z ciągu znaków, ponieważ używali ich bezpośrednio.

Obecnie istnieje wiele różnych formatów daty, a także różne sposoby wyświetlania dat w zależności od lokalizacji i preferencji użytkownika. Dlatego parsowanie dat jest ważnym krokiem w przetwarzaniu danych i programiści muszą umieć to robić.

W języku TypeScript jest wiele sposobów na parsowanie dat z ciągu znaków. Można to zrobić za pomocą wbudowanego obiektu Date lub wykorzystując biblioteki takie jak Moment.js lub Day.js.

## Zobacz także:
- [Dokumentacja Date w języku TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-2.html#date-built-in)
- [Biblioteka Moment.js](https://momentjs.com/)
- [Biblioteka Day.js](https://day.js.org/)