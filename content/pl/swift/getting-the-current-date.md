---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:51.855993-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
(Po co i dlaczego?)
Pobieranie bieżącej daty pozwala nam reagować na czas rzeczywisty. Używamy tego w logowaniu, w odmierzaniu czasu, w harmonogramach – wszędzie, gdzie czas jest kluczowy.

## How to:
(Jak to zrobić:)
```Swift
import Foundation

// Pobranie bieżącej daty i czasu
let currentDate = Date()

// Wydrukowanie bieżącej daty i czasu do konsoli
print(currentDate)
```
Przykładowe wyjście:
```
2023-04-14 07:38:46 +0000
```

## Deep Dive:
(Zagłębiając się:)
Pobranie bieżącej daty w Swift jest proste, ale kryje w sobie więcej zaawansowanych możliwości. W roku 1998, poprzednik Swifta, Objective-C, był już wykorzystywany do obsługi dat. Swift przyniósł uproszczenie dzięki typowi `Date`.

W alternatywie, możemy użyć `Calendar` do bardziej złożonych operacji z datami, jak różnice czasowe czy formatowanie. `DateFormatter` pozwala nam konwertować daty na czytelne stringi i odwrotnie, biorąc pod uwagę lokalizacje i preferencje użytkownika.

W praktyce, warto też pamiętać o strefach czasowych (`TimeZone`) i ustawieniach lokalnych (`Locale`), aby czas był właściwie interpretowany na całym świecie.

```Swift
import Foundation

// Przykład formatowania daty
let formatter = DateFormatter()
formatter.dateStyle = .medium
formatter.timeStyle = .medium

let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```
Przykładowe wyjście dla lokalizacji w USA:
```
Apr 14, 2023, 7:38:46 AM
```

## See Also:
(Zobacz też:)
- Dokumentacja Swift `Date`: https://developer.apple.com/documentation/foundation/date
- Przewodnik po `DateFormatter`: https://developer.apple.com/documentation/foundation/dateformatter
- Informacje o `Calendar` w Swift: https://developer.apple.com/documentation/foundation/calendar