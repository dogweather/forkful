---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Swift: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu określonej ilości dni, miesięcy lub lat do lub od danej daty. Programiści robią to dla wielu celów, takich jak harmonogramy, przypomnienia czy terminy wygaśnięcia licencji.

## Jak to zrobić:

Twój kod Swift do obliczania daty w przyszłości lub przeszłości może wyglądać tak:

```Swift
import Foundation

let dzis = Date()
let kalendarz = Calendar.current

if let dataPrzyszla = kalendarz.date(byAdding: .day, value: 7, to: dzis) {
    print("Data za tydzień to: \(dataPrzyszla)")
}

if let dataPrzeszla = kalendarz.date(byAdding: .year, value: -1, to: dzis) {
    print("Data rok temu to: \(dataPrzeszla)")
}
```
Output:

```Swift
Data za tydzień to: 2022-07-23 13:59:30 +0000
Data rok temu to: 2021-07-16 13:59:30 +0000
```

## Pogłębione Informacje

1. Kontekst historyczny: Koncepcja obliczania daty w przyszłości lub przeszłości istnieje od dawna, a narzędzia do tego celu różniły się od prostych kalendarzy papierowych po zaawansowane aplikacje i algorytmy.

2. Alternatywy: W Swift, możemy korzystać z innych typów, takich jak `NSDateComponents` do obliczania daty, choć `Calendar` jest zdecydowanie bardziej użyteczne i łatwe w użyciu.

3. Szczegóły implementacji: Funkcja `date(byAdding:value:to:)` klasy `Calendar` dodaje określoną liczbę jednostek (dni, miesięcy, lat) do danej daty. Operuje na podstawie kalendarza gregoriańskiego, ale może być dostosowana do innych systemów kalendarzowych.

## Zobacz Również

Zapoznaj się z poniższymi źródłami, aby dowiedzieć się więcej:

1. Dokumentacja Apple na temat klasy `Calendar`: <https://developer.apple.com/documentation/foundation/calendar>
2. Przewodnik po manipulacji datą i czasem w Swift: <https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-arent-in-the-books>
3. Tutorial o zarządzaniu datą i czasem w Swift: <https://www.appcoda.com/date-time-classes-swift/>