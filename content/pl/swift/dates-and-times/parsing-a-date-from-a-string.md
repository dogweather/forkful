---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases: - /pl/swift/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:43.926212-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty ze stringa polega na konwertowaniu tekstowych reprezentacji daty i czasu na obiekt `Date`. Proces ten jest niezbędny w aplikacjach, w których daty przekazywane są jako ciągi znaków, np. w odpowiedziach API czy wprowadzanych przez użytkownika, co umożliwia łatwiejszą manipulację i formatowanie dat.

## Jak to zrobić:

### Używając `DateFormatter` z biblioteki Foundation
Standardowa biblioteka Swifta, Foundation, dostarcza `DateFormatter` do konwertowania stringów na obiekty `Date` i vice versa. Aby sparsować datę ze stringa, należy określić format daty odpowiadający stringowi, a następnie użyć formatera do jej parsowania.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Sparsowana data: \(date)")
} else {
    print("Nie udało się sparsować daty")
}
// Przykładowy wynik: Sparsowana data: 2023-04-29 22:00:00 +0000
```

Należy zauważyć, że wynik może się różnić w zależności od twojej strefy czasowej.

### Używając `ISO8601DateFormatter`
Dla formatów dat ISO 8601, Swift dostarcza specjalizowany formater, `ISO8601DateFormatter`, który upraszcza proces parsowania.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Sparsowana data ISO8601: \(date)")
} else {
    print("Nie udało się sparsować daty ISO8601")
}
// Przykładowy wynik: Sparsowana data ISO8601: 2023-04-30 15:00:00 +0000
```

### Używając biblioteki zewnętrznej: SwiftDate
Chociaż Swift zapewnia solidne narzędzia do parsowania dat, biblioteki zewnętrzne jak SwiftDate oferują jeszcze większą elastyczność i wygodę. Po dodaniu SwiftDate do projektu, parsowanie staje się tak proste jak:

```swift
import SwiftDate

let dateString = "30 kwietnia 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Data sparsowana za pomocą SwiftDate: \(date)")
} else {
    print("Nie udało się sparsować daty za pomocą SwiftDate")
}
// Przykładowy wynik: Data sparsowana za pomocą SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate upraszcza parsowanie z użyciem języka naturalnego i szerokiego zakresu formatów dat, czyniąc go potężnym dodatkiem do zestawu narzędzi programistycznych Swift.
