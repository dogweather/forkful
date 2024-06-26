---
date: 2024-01-20 17:33:53.911764-07:00
description: "How to: (Jak to zrobi\u0107?) Por\xF3wnajmy dwie daty w Swift."
lastmod: '2024-04-05T21:53:37.193877-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) Por\xF3wnajmy dwie daty w Swift."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## How to:
(Jak to zrobić?)

Porównajmy dwie daty w Swift:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

let date1 = dateFormatter.date(from: "2023/04/15 08:30")!
let date2 = dateFormatter.date(from: "2023/04/16 21:45")!

// Porównanie dat
if date1 == date2 {
    print("Daty są identyczne.")
} else if date1 < date2 {
    print("Data pierwsza jest wcześniejsza.")
} else {
    print("Data druga jest wcześniejsza.")
}

// Wynik:
// Data pierwsza jest wcześniejsza.
```

## Deep Dive:
(Głębsze zagłębienie)

Wczesne systemy programowania nie miały wbudowanego wsparcia dla zarządzania datą i czasem. Pokutujące jeszcze dziś systemy, jak UNIX Time, wprowadzały standardy. Obecnie Swift oferuje typy `Date` i `Calendar`, które ułatwiają pracę z czasem. Alternatywy? Można też użyć timestampów lub `TimeInterval`. Zaawansowane opcje to np. `Calendar.current.compare()` do porównywania komponentów daty, co daje większą elastyczność.

## See Also:
(Zobacz też)

- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
