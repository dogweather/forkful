---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:38:31.578814-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing daty z ciągu znaków oznacza przekształcanie tekstowego zapisu daty na typ danych rozumiany przez program. Programiści robią to, aby łatwo obsługiwać i manipulować datami w logice aplikacji.

## How to: (Jak to zrobić:)
Swift dostarcza silne narzędzia do zarządzania datami. Poniżej znajdziesz prosty przykład konwersji stringa do daty.

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateStr = "2023-03-21 19:45:00"

if let date = dateFormatter.date(from: dateStr) {
    print("Data: \(date)")
} else {
    print("Nie udało się przekształcić daty.")
}
```

Wyjście:
```
Data: 2023-03-21 19:45:00 +0000
```

## Deep Dive (Dogłębna analiza)
Daty bywają podchwytliwe. Formaty są różne w zależności od regionu; stąd `DateFormatter` w Swift umożliwia ustawienie lokalizacji (`locale`) i strefy czasowej (`timeZone`). W historycznym kontekście, różne systemy używały różnych kalendarzy, ale ISO8601 stał się międzynarodową normą. Alternatywą dla `DateFormatter` jest używanie `ISO8601DateFormatter`, jeśli pracujesz ze standardowym formatem ISO. W przeszłości, manipulacje datami bywały trudniejsze i mniej intuicyjne, ale Swift uprościł ten proces, choć nadal wymaga od programistów znajomości pewnych niuansów, jak np. różnorodność formatów.

## See Also (Zobacz również)
- [Swift Documentation on Date](https://developer.apple.com/documentation/foundation/date)
