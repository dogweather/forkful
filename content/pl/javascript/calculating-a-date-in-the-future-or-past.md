---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Javascript: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przeszłości lub przyszłości to sposób na zdobycie informacji o dacie, która jest wcześniej lub później niż aktualna data. Programiści robią to, aby przewidzieć wydarzenia, zarządzać harmonogramami lub obsługiwać terminy.

## Jak to zrobić:

Obliczanie daty w przyszłości lub przeszłości nie jest trudne. Oto prosty przykład:

```Javascript
let dataTeraz = new Date();
console.log("Dzisiaj: " + dataTeraz);

// Dodanie 5 dni do obecnej daty
dataTeraz.setDate(dataTeraz.getDate() + 5);
console.log("Za 5 dni: " + dataTeraz);

// Odejmowanie 7 dni od obecnej daty
dataTeraz.setDate(dataTeraz.getDate() - 7);
console.log("7 dni temu: " + dataTeraz);
```

Możliwy wynik tego kodu to:

```Javascript
"Dzisiaj: Thu Sep 16 2021 12:34:56 GMT+0200 (Central European Summer Time)"
"Za 5 dni: Tue Sep 21 2021 12:34:56 GMT+0200 (Central European Summer Time)"
"7 dni temu: Tue Sep 14 2021 12:34:56 GMT+0200 (Central European Summer Time)"
```

## Więcej szczegółów:

Obliczanie daty w przeszłości i przyszłości jest popularne, ale było trudne do zrealizowania na początku ery komputerowej z powodu ograniczeń pamięci i procesora.

Alternatywą dla metody setDate() w JavaScript jest użycie zewnętrznej biblioteki, jak moment.js, która oferuje większą precyzję i polepsza sterowanie datą.

Ważnym aspektem implementacji jest to, że setDate() uwzględnia przepełnienia. Na przykład, jeśli do daty 31 stycznia dodasz jeden dzień, wynikiem będzie 1 lutego.

## Zobacz także:

1. [JavaScript Date Reference na stronie MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Biblioteka Moment.js](https://momentjs.com/)
3. [JavaScript Date Tutorial na W3Schools](https://www.w3schools.com/js/js_date_methods.asp)