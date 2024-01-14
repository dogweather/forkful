---
title:                "Swift: Konwertowanie daty na ciąg znaków."
simple_title:         "Konwertowanie daty na ciąg znaków."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Czemu

Konwersja daty na łańcuch znaków może być użyteczna, gdy chcesz wyświetlić datę w czytelny dla użytkownika sposób lub zapisać ją w pliku tekstowym.

## Jak To Zrobić

```Swift 
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let dateString = dateFormatter.string(from: date)

print(dateString)
// Output: 30.10.2021 
```

Najpierw tworzymy obiekt typu `Date`, który przechowuje aktualną datę. Następnie tworzymy obiekt `DateFormatter` i ustawiamy format daty, w tym przypadku `dd.MM.yyyy`. Na koniec wywołujemy metodę `string(from:)` obiektu `DateFormatter`, aby przekonwertować naszą datę na łancuch znaków i przypisujemy go do zmiennej `dateString`. Wypisując tę zmienną, otrzymamy sformatowaną datę jako wynik.

## Głębszy Przegląd

Konwersja daty na łańcuch znaków jest możliwa dzięki obiektowi `DateFormatter`, który odpowiada za formatowanie i wyświetlanie daty. Funkcja `dateFormat` jest jednym z wielu formatów dostępnych do wyboru, w zależności od preferencji użytkownika lub specyfikacji projektu.

Możemy również wykorzystać funkcję `string(with:)`, aby wyświetlić tylko część daty, na przykład tylko dzień tygodnia lub miesiąc. Ważne jest, aby uważnie wybierać format daty, ponieważ różne kraje mogą mieć różne konwencje datowania.

## Zobacz Również

- Dokumentacja Apple dla DateFormatter: https://developer.apple.com/documentation/foundation/dateformatter
- Przewodnik po formatach daty: https://nsdateformatter.com/