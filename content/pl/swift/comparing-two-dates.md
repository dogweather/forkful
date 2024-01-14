---
title:                "Swift: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być bardzo przydatne w programowaniu. Może to pomóc nam w ustalaniu, które zdarzenia wystąpiły wcześniej, porównaniu czasu trwania zdarzeń oraz w wielu innych sytuacjach. W tym artykule dowiesz się, jak porównywać dwie daty w języku Swift.

## Jak

Poniżej znajdują się przykładowe kody, które pomogą ci w porównywaniu dwóch dat w języku Swift.

```Swift
// Porównanie dwóch dat z użyciem funkcji ">", "<", ">=", "<="
let firstDate = Date("2020-01-01")
let secondDate = Date("2021-01-01")
if firstDate > secondDate {
    print("\(firstDate) jest późniejszą datą niż \(secondDate)")
} else {
    print("\(secondDate) jest późniejszą datą niż \(firstDate)")
}

// Porównanie dwóch dat z użyciem funkcji "compare"
let result = firstDate.compare(secondDate)
if result == .orderedAscending {
    print("\(firstDate) jest wcześniejszą datą niż \(secondDate)")
} else if result == .orderedDescending {
    print("\(firstDate) jest późniejszą datą niż \(secondDate)")
} else {
    print("\(firstDate) i \(secondDate) są takie same")
}
```

W powyższych przykładach użyliśmy funkcji `Date` do utworzenia daty oraz funkcji `compare` do porównania dwóch dat. Funkcja `compare` zwraca wartość `orderedAscending`, `orderedDescending` lub `orderedSame`, w zależności od wyniku porównania dat.

Możemy również wykorzystać dodatkowe metody do porównywania dat, takie jak `isDate(_:equalTo:)`, `isDate(_:inSameDayAs:)` czy `isDate(_:inSameYearAs:)`. Wszystkie one są dostępne w klasie `Calendar`.

## Deep Dive

Dowiedzieliśmy się już, jak porównywać dwie daty w języku Swift, ale warto również poznać nieco więcej o samych datach.

W języku Swift, daty są reprezentowane przez strukturę `Date`, która przechowuje informacje o liczbach sekund od stałej daty punktu odniesienia (1 stycznia 2001 roku). Możemy również korzystać z klasy `Calendar`, która pomaga nam w manipulowaniu datami i czasami, jak również wyznaczaniu odpowiedniego punktu odniesienia.

Kluczowym punktem w porównywaniu dat jest punkt odniesienia, który wyznacza początek liczenia czasu. W języku Swift, domyślnie jest to rok 2001, ale możemy zmienić ten punkt odniesienia, korzystając z klasy `Calendar`.

## Zobacz również

- Dokumentacja do klasy `Date` w języku Swift: https://developer.apple.com/documentation/foundation/date
- Dokumentacja do klasy `Calendar` w języku Swift: https://developer.apple.com/documentation/foundation/calendar
- Wprowadzenie do dat i czasów w języku Swift: https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html