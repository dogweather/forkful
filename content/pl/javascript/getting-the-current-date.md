---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:15:33.335088-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Pobieranie bieżącej daty to odczytanie daty i czasu, w którym znajduje się system. Programiści wykorzystują to do logowania, ograniczeń czasowych funkcji, czy personalizacji treści.

## How to: (Jak to zrobić:)
```Javascript
// Pobranie bieżącej daty i czasu
let currentDate = new Date();

// Wyświetlenie bieżącej daty i czasu
console.log(currentDate);
```
Sample output:
```
2023-03-17T12:45:30.789Z
```
Aby uzyskać bardziej czytelny format:
```Javascript
// Pobranie i formatowanie daty do postaci YYYY-MM-DD
let date = currentDate.toISOString().split('T')[0];

// Wyświetlenie sformatowanej daty
console.log(date);
```
Sample output:
```
2023-03-17
```

## Deep Dive (Dogłębna analiza)
JavaScript od lat 90 używa obiektu `Date` do obsługi dat i czasu. Mimo że istnieją nowocześniejsze biblioteki jak Moment.js czy date-fns, `Date` pozostaje natywnym i wszechstronnym narzędziem.

`new Date()` tworzy obiekt z aktualną datą i czasem, zależnym od strefy czasowej systemu. `toISOString()` konwertuje datę na format ISO 8601 (np. 2023-03-17T12:45:30.789Z). Dzielenie wyniku metody `split('T')` pozwala oddzielić datę od czasu.

Zaletą `Date` jest brak zewnętrznych zależności, ale może wymagać więcej kodu do prostych zadań i nie obsługuje stref czasowych tak łatwo jak inne biblioteki. Z drugiej strony, użycie zewnętrznych bibliotek może nie być potrzebne dla prostych zastosowań i może wprowadzać niepotrzebny narzut.

## See Also (Zobacz także)
- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs)
- [date-fns library](https://date-fns.org/)
