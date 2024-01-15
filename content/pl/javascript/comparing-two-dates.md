---
title:                "Porównanie dwóch dat."
html_title:           "Javascript: Porównanie dwóch dat."
simple_title:         "Porównanie dwóch dat."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być niezbędnym krokiem, jeśli tworzysz aplikację, która wymaga przetwarzania i zarządzania datami. Dzięki porównywaniu dat możesz ustalać kolejność wydarzeń, wyświetlać najnowsze informacje lub sprawdzać, czy dana data jest przeszła lub przyszła. Jest to również bardzo przydatne podczas analizowania danych historycznych lub planowania przyszłych działań.

## Jak to zrobić

```Javascript
// Stwórz dwie zmienne daty
const date1 = new Date('2020-01-01');
const date2 = new Date('2020-02-01');

// Porównaj daty przy użyciu operatorów logicznych
if (date1 > date2) {
  console.log('Data 1 jest późniejsza niż data 2');
} else if (date1 < date2) {
  console.log('Data 2 jest późniejsza niż data 1');
} else {
  console.log('Obie daty są takie same');
}

// Wynik: Data 2 jest późniejsza niż data 1
```

Możesz również skorzystać z wbudowanych metod w obiektach dat, takich jak `getTime()` lub `getTimezoneOffset()`, aby uzyskać informacje o czasie i porównać różnice między datami.

## Głębszy zanurzenie

Każda data w języku Javascript jest przechowywana w postaci liczby milisekund, które upłynęły od 1 stycznia 1970 roku. Oznacza to, że porównując daty, porównujesz w rzeczywistości wartości liczbowe. W przypadku operatorów logicznych, takich jak `>`, `>=`, `<`, `<=` daty są przekształcane do tego samego typu, dzięki czemu możesz je porównywać bez problemów.

Możesz również wykorzystać wbudowane metody w obiektach dat, takie jak `getTime()`, aby uzyskać wartość liczbową reprezentującą datę. Dzięki temu możesz wykonać różne operacje matematyczne, takie jak dodawanie lub odejmowanie różnych jednostek czasu.

## Zobacz także

- [MDN - Javascript Date Object](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Javascript Date Library](https://www.w3schools.com/js/js_dates.asp)
- [Stack Overflow - How to Compare two Dates in JavaScript](https://stackoverflow.com/questions/5223/length-of-a-javascript-object/5224#5224)