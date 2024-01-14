---
title:                "Swift: Porównywanie dwóch dat"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

Wprowadzenie do porównywania dwóch dat w języku Swift

## Dlaczego

Porównywanie dat jest nieodłączną częścią wielu aplikacji, które wymagają śledzenia i analizowania czasu. Może to być przydatne w wielu różnych scenariuszach, takich jak planowanie zadań, rezerwacja biletów lub monitorowanie zmian w bazie danych. Porównywanie dat pomaga w ustaleniu, która data jest późniejsza lub wcześniejsza, a także w obliczeniu różnicy między nimi.

## Jak to zrobić

Porównywanie dat w języku Swift jest bardzo proste i wykorzystuje wewnętrzne funkcje języka. Aby porównać dwie daty, należy użyć operatora porównania `>` lub `<`, w zależności od tego, czy chcemy sprawdzić, która data jest późniejsza lub wcześniejsza. Poniżej przedstawiono przykładowy kod porównujący dwie daty:

```Swift
let today = Date()

let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today) // tworzy datę na jutro

if tomorrow! > today {
    print("Jutro jest późniejsze niż dziś!")
}
```

W powyższym przykładzie porównujemy dzisiejszą datę z datą jutra. W celu utworzenia daty na jutro używamy metody `date(byAdding:)` na kalendarzu i przypisujemy ją do stałej `tomorrow`. W linii warunku sprawdzamy, czy data jutra jest późniejsza niż dzisiejsza, a następnie wyświetlamy odpowiedni komunikat.

## Deep Dive

Istnieje wiele metod i właściwości, które można wykorzystać do bardziej zaawansowanego porównywania dat w języku Swift. Jedną z nich jest `compare(_:to:toGranularity:)`, która porównuje dwie daty i zwraca wartość typu `ComparisonResult` wskazującą na to, czy pierwsza data jest wcześniejsza, późniejsza lub równa drugiej dacie. Możliwe jest również ustawienie precyzji porównywania, np. do miesiąca, dnia, godziny lub minuty.

## Zobacz także

- Dokumentacja Apple na temat porównywania dat w języku Swift: https://developer.apple.com/documentation/foundation/date
- Tutoriale i przykłady dotyczące porównywania dat w Swift: https://www.swiftbysundell.com/articles/working-with-dates-in-swift/
- Wskazówki i triki dotyczące porównywania dat w języku Swift: https://marcosantadev.com/categories/#dates-times