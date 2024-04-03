---
date: 2024-01-20 17:33:53.911764-07:00
description: "(Po co i dlaczego?) Por\xF3wnywanie dat pozwala okre\u015Bli\u0107,\
  \ kt\xF3ra z nich jest wcze\u015Bniejsza, a kt\xF3ra p\xF3\u017Aniejsza. Programi\u015B\
  ci robi\u0105 to, by zarz\u0105dza\u0107 czasem i\u2026"
lastmod: '2024-03-13T22:44:35.767172-06:00'
model: gpt-4-1106-preview
summary: (Po co i dlaczego.
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## What & Why?
(Po co i dlaczego?)

Porównywanie dat pozwala określić, która z nich jest wcześniejsza, a która późniejsza. Programiści robią to, by zarządzać czasem i wydarzeniami, obsługiwać harmonogramy, upływ czasu, czy ważność tokenów.

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
