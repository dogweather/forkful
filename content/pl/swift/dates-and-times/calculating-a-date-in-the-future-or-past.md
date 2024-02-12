---
title:                "Obliczanie daty w przyszłości lub przeszłości"
aliases:
- /pl/swift/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:32:22.584825-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obliczanie daty w przyszłości lub przeszłości to proces ustalenia daty, dodając lub odejmując pewien czas od bieżącej daty. Programiści wykonują to zadanie, aby obsłużyć przedział czasowy dla wydarzeń, przypomnień, ważności subskrypcji, i wielu innych funkcji związanych z czasem.

## Jak to zrobić:
```Swift
import Foundation

// Obecna data
let today = Date()

// Kalendarz
var calendar = Calendar.current

// Dodajmy 3 dni
if let threeDaysLater = calendar.date(byAdding: .day, value: 3, to: today) {
    print("Trzy dni później: \(threeDaysLater)")
}

// Odejmijmy 5 lat
if let fiveYearsEarlier = calendar.date(byAdding: .year, value: -5, to: today) {
    print("Pięć lat wcześniej: \(fiveYearsEarlier)")
}
```
Przykładowe wyjście:
```
Trzy dni później: 2023-07-19 14:22:35 +0000
Pięć lat wcześniej: 2018-07-16 14:22:35 +0000
```

## Głębsze spojrzenie:
Kalkulacje dat to znana praktyka od gdy kalendarze stały się narzędziami używanymi na co dzień. W informatyce obliczenia czasu są kluczowe dla logów, timestampów i planowania. Swift korzysta z `Date` oraz `Calendar` - to dwie główne klasy używane do manipulowania czasem. Istnieją alternatywy, jak na przykład `TimeIntervalSince1970` dla Unix Timestamp lub stosowne biblioteki trzecich stron jak `SwiftDate`, które oferują więcej funkcji. Szczegóły implementacyjne zależą od konkretnego kalendarza, strefy czasowej użytkownika oraz systemu szacowania przebiegu czasu, które mogą komplikować obliczenia, szczególnie dla czasu datowanego w przeszłości.

## Zobacz także:
- [Apple Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [SwiftDate library](https://github.com/malcommac/SwiftDate)
- [NSHipster Article on Date and Time](https://nshipster.com/datecomponents/)
