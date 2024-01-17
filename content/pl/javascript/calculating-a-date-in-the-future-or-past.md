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
Obliczanie daty w przyszłości lub przeszłości jest procesem, w którym dzięki użyciu programowania i odpowiednich funkcji, jesteśmy w stanie wyświetlić lub obliczyć daty z przyszłości lub przeszłości. Programiści często tego używają w celu tworzenia dynamicznych aplikacji, takich jak kalendarze, alarmy lub planery.

## Jak to zrobić:
Możemy wykorzystać funkcję `getDate()` do pobrania aktualnej daty oraz funkcję `setDate()` do ustawienia nowej daty. Poniżej znajduje się przykładowy kod, który oblicza datę 7 dni w przyszłości i wypisuje ją w konsoli.

```Javascript
const currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 7);
console.log(currentDate);
```

Output: **Tue Mar 30 2021 10:45:56 GMT+0200 (Central European Summer Time)**

Możemy także użyć biblioteki takiej jak Moment.js, która posiada wiele funkcji do manipulowania datami w prostszy sposób. Przykładowy kod wykorzystujący tę bibliotekę wyglądałby następująco:

```Javascript
const currentDate = moment();
const futureDate = currentDate.add(7, 'days').format("DD/MM/YYYY");
console.log(futureDate);
```

Output: **30/03/2021**

## Głębsza analiza:
Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki tzw. czasowi UNIX-owemu, który jest używany w systemach operacyjnych i oparty jest na liczbie sekund od 1 stycznia 1970 roku. Istnieje również wiele alternatywnych bibliotek i funkcji, które umożliwiają obliczanie dat, np. date-fns czy Luxon. Ważne jest również uwzględnienie stref czasowych i innych czynników, które mogą wpłynąć na obliczenie dokładnej daty.

## Zobacz również:
- [MDN Web Docs - Object Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Moment.js - dokumentacja](https://momentjs.com/docs/)
- [date-fns - dokumentacja](https://date-fns.org/docs/Getting-Started)
- [Luxon - dokumentacja](https://moment.github.io/luxon/index.html)