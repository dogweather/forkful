---
title:    "Swift: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego?

Obliczanie daty w przeszłości lub przyszłości może być bardzo przydatne w programowaniu. Może nam to pomóc w tworzeniu aplikacji związanych z kalendarzem, planowaniu zadań lub wyświetlania aktualnych danych.

## Jak to zrobić?

W Swift istnieje wiele różnych metod i funkcji, które możemy wykorzystać do obliczania daty w przyszłości lub przeszłości. Poniżej znajdują się przykłady kodu w języku Swift, których wynikiem będzie wyświetlenie daty w przeszłości lub przyszłości w konsoli.

```swift
// Obliczanie daty w przeszłości o 7 dni
let calendar = Calendar.current
let today = Date()
let oneWeekAgo = calendar.date(byAdding: .day, value: -7, to: today)
print(oneWeekAgo)

// Obliczanie daty w przyszłości o 1 miesiąc
let nextMonth = calendar.date(byAdding: .month, value: 1, to: today)
print(nextMonth)

// Obliczanie daty w przeszłości o 1 rok
let lastYear = calendar.date(byAdding: .year, value: -1, to: today)
print(lastYear)
```

W powyższych przykładach wykorzystujemy klasę `Calendar` oraz jej metodę `date(byAdding:to:)`, która pozwala nam na dodawanie lub odejmowanie odpowiedniej wartości czasu do wybranego dnia. Możemy również wykorzystać inne wartości, takie jak `week`, `hour` czy `minute`, aby obliczyć datę w dowolnym zakresie czasowym.

## Głębszy wgląd

Obliczanie daty w przyszłości lub przeszłości może być bardziej skomplikowane, jeśli chcemy uwzględnić różne strefy czasowe. Wtedy musimy korzystać z klasy `DateFormatter` oraz ustawiać odpowiednie ustawienia dla naszej daty.

Możemy również wykorzystać różne biblioteki i frameworki, takie jak `DateTools` czy `SwiftDate`, które oferują nam więcej funkcjonalności i ułatwiają pracę z datami.

## Zobacz również

- [Dokumentacja o klasie Date w języku Swift](https://developer.apple.com/documentation/foundation/date)
- [Biblioteka DateTools w języku Swift](https://github.com/MatthewYork/DateTools)
- [Framework SwiftDate w języku Swift](https://github.com/malcommac/SwiftDate)