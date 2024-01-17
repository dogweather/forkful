---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "TypeScript: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dla czego?

Obliczenie daty w przyszłości lub przeszłości jest jedną z najczęstszych czynności wykonywanych przez programistów. Polega ona na wykorzystaniu wcześniej podanej daty i dodaniu (lub odjęciu) odpowiedniej liczby dni, tygodni, miesięcy lub lat, aby uzyskać przyszłą lub przeszłą datę. Programiści wykonują tę czynność, aby móc operować na różnych przedziałach czasowych i dokonywać obliczeń na dacie.

## Jak to zrobić:

TypeScript jest językiem programowania, który umożliwia obliczanie daty w przyszłości lub przeszłości w bardzo prosty sposób. Wystarczy użyć wbudowanego obiektu "Date" i jego metody "setDate" lub "getDate", aby ustawić lub odczytać daną część daty. Poniżej znajdują się przykładowe kody w TypeScript oraz odpowiadające im wyniki:

```
// Ustawienie daty 10 dni w przyszłości
const currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 10);
console.log(currentDate); // Output: Wed Jan 20 2021 20:25:00 GMT+0100 (czas środkowoeuropejski standardowy)

// Odczytanie dnia tygodnia dla przyszłej daty
const futureDate = new Date('2021-02-14');
const dayOfWeek = futureDate.getDay();
console.log(dayOfWeek); // Output: 0 (niedziela)
```

## Głębszy wykład:

Obliczanie daty w przyszłości lub przeszłości jest bardzo przydatną umiejętnością w programowaniu i jest wykorzystywane w wielu dziedzinach, takich jak tworzenie aplikacji do zarządzania czasem, systemów rezerwacji lub serwisów społecznościowych. Istnieje wiele alternatywnych sposobów na obliczanie daty, takich jak użycie bibliotek zewnętrznych czy korzystanie z różnych formatów daty w języku TypeScript. Warto również pamiętać o uwzględnieniu różnych stref czasowych, aby obliczenia były dokładne i nie powodowały niepotrzebnych błędów.

## Zobacz również:

- Dokumentacja TypeScript dla obiektu "Date": https://www.typescriptlang.org/docs/handbook/standard-library.html#date
- Przewodnik po korzystaniu z dat w języku TypeScript: https://www.zaniyahstudio.com/2017/10/20/managing-dates-with-typescript/
- Biblioteka Moment.js dla łatwiejszego zarządzania datami w języku JavaScript i TypeScript: https://momentjs.com/