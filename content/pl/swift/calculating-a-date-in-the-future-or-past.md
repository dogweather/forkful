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

## Dlaczego

Często zdarza się, że musimy obliczyć datę w przeszłości lub przyszłości, na przykład w celu wyświetlenia terminu ważności produktu lub wydarzenia. W tym artykule zaprezentuję prosty sposób na wykonanie tej operacji w języku Swift.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Swift, możemy użyć klasy `Date`. Przykłady kodu znajdują się poniżej:

```Swift
// Obliczenie daty 7 dni w przyszłości
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: Date())

// Obliczenie daty 2 miesiące w przeszłości
let pastDate = Calendar.current.date(byAdding: .month, value: -2, to: Date())

// Formatowanie daty za pomocą DateFormatter
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let formattedDate = dateFormatter.string(from: futureDate!)

print(formattedDate)
// Output: 25.05.2021
```

Możemy także wyświetlić datę w innych formatach, takich jak rok, miesiąc, dzień, godzina lub minuta. Aby poznać więcej możliwości manipulacji datą w języku Swift, warto zapoznać się z dokumentacją klasy `Date`.

## Wnikliwa analiza

Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki temu, że klasa `Date` jest oparta na liczbach całkowitych. Dzięki temu możemy dodawać lub odejmować odpowiednią wartość od bieżącej daty w celu uzyskania oczekiwanej daty. Dodatkowo, formatowanie daty za pomocą klasy `DateFormatter` pozwala nam dostosować wyświetlanie daty według naszych preferencji.

## Zobacz także

- [Dokumentacja klasy Date w języku Swift](https://developer.apple.com/documentation/foundation/date)
- [Dokumentacja klasy DateFormatter w języku Swift](https://developer.apple.com/documentation/foundation/dateformatter)