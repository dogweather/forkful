---
title:    "Swift: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych programach coraz częściej musimy operować na datach. Czasami potrzebujemy obliczyć datę w przyszłości lub w przeszłości w oparciu o aktualną datę. W tym wpisie dowiesz się, jak wykonać tę operację w języku Swift.

## Jak to zrobić

```Swift
// Obliczanie daty, 20 dni w przyszłości
let currentDate = Date()
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .day, value: 20, to: currentDate)
print(futureDate) // Wypisze: "Optional(2021-10-10 16:06:24 +0000)"
```

Ten prosty kod używa klasy `Date` i obiektu `Calendar.current`, aby obliczyć datę 20 dni od aktualnego dnia. Ta metoda może być również używana do obliczania daty w przeszłości poprzez zmianę wartości parametru `value` na ujemną. 

## Deep Dive

W języku Swift, daty są przechowywane jako obiekty typu `Date`. Aby manipulować datami, musimy być w stanie je przetwarzać. Klasa `Calendar` jest odpowiedzialna za wykonywanie operacji związanych z datami. Gdy chcemy obliczyć datę w przyszłości lub w przeszłości, możemy użyć metody `date(byAdding:to:wrappingComponents)`, gdzie pierwszy parametr określa, jaką jednostkę czasu chcemy dodać lub odjąć, np. rok, miesiąc, dzień, a drugi parametr to data odniesienia. 

## Zobacz również
- [Dokumentacja Apple: Klasa Date](https://developer.apple.com/documentation/foundation/date)
- [Dokumentacja Apple: Klasa Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Wideo na YouTube: Obliczanie dat w języku Swift](https://www.youtube.com/watch?v=Q_BZsopxQHI)