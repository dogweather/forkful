---
title:    "Swift: Uzyskiwanie aktualnej daty"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego
Często w programowaniu potrzebujemy informacji o obecnym dniu, na przykład do wyświetlenia daty ostatniej modyfikacji pliku lub określenia daty ważności danego zadania. Dlatego pobieranie obecnej daty jest ważną umiejętnością, która ułatwia pracę programisty.

## Jak to zrobić
Aby pobrać obecną datę w języku Swift, należy skorzystać z klasy `Date` oraz metody `init()` bez argumentów. Następnie można wyświetlić datę w różnych formatach za pomocą obiektu klasy `DateFormatter`. Na przykład:

```Swift
let currentDate = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let formattedDate = formatter.string(from: currentDate)

// Wynik: 02.07.2021
```

## Głębszy wgląd
Klasa `Date` jest częścią biblioteki Foundation i reprezentuje liczbę sekund od 1 stycznia 2001 roku. Metoda `init()` bez argumentów tworzy obiekt z aktualną datą i czasem, ale można również podać konkretną datę lub czas jako argument. Klasa `DateFormatter` pozwala na dostosowywanie formatu daty, używając odpowiednich symboli (np. "dd" - dzień, "MM" - miesiąc).

## Zobacz też
- Apple Developer Documentation - [Class Date](https://developer.apple.com/documentation/foundation/date)
- Apple Developer Documentation - [Class DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Swift by Sundell - [Working with dates and times in Swift](https://www.swiftbysundell.com/articles/working-with-dates-and-times-in-swift/)