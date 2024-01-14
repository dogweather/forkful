---
title:                "Swift: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub w przeszłości jest niezbędnym elementem wielu aplikacji. Może być wykorzystywane do wyświetlania planów, przeliczania czasu lub ustalania terminów płatności. Dzięki temu możemy zapewnić użytkownikom bardziej dopasowaną i personalizowaną obsługę.

## Jak to zrobić

```Swift
// Ustawienie aktualnej daty
let currentDate = Date()

// Ustawienie liczby dni, miesięcy lub lat do dodania lub odjęcia od aktualnej daty
let future = 3

// Utworzenie instancji obiektu wyliczającego datę w przyszłości na podstawie aktualnej daty i liczby dni
let futureDate = Calendar.current.date(byAdding: .day, value: future, to: currentDate)

// Ustawienie formatu daty i konwersja wyniku na string
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let futureDateString = dateFormatter.string(from: futureDate!)

// Wyświetlenie wyniku
print("Data za \(future) dni to: \(futureDateString)")
```

Przykładowy wynik: Data za 3 dni to: 26-05-2021

## Głębsza analiza

Aby obliczyć datę w przyszłości lub w przeszłości, należy utworzyć obiekt typu Date, który odpowiada za przechowywanie daty. Następnie, przy użyciu metody `date(byAdding:value:to:)` możemy dodać lub odjąć od niej odpowiednią ilość dni, miesięcy lub lat. Ważnym elementem jest również właściwe ustawienie formatu daty za pomocą obiektu DateFormatter.

## Zobacz również

- Dokumentacja Apple Developer dotycząca obliczania daty: https://developer.apple.com/documentation/foundation/calendar/2293543-date
- Tutoriale Swift dla początkujących: https://www.raywenderlich.com/493-brainwash-tutorial-first-steps-with-the-swift-language
- Przegląd funkcji kalendarza w języku Swift: https://www.twilio.com/blog/a-swift-look-at-the-calendar-api