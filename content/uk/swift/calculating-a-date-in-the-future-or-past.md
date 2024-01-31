---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:32:35.280184-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і для чого?
Обчислення дати у майбутньому чи минулому — це процес знаходження дати, яка відбудеться через певний час або вже була. Програмісти роблять це для функцій планування, відстеження часу та задач, що залежать від дати.

## Як робити:
```Swift
import Foundation

let now = Date()
var dateComponents = DateComponents()
dateComponents.day = 10  // Для прикладу виберемо 10 днів

// Обчислюємо дату в майбутньому
if let tenDaysLater = Calendar.current.date(byAdding: dateComponents, to: now) {
    print("Десять днів після сьогодні: \(tenDaysLater)")
} else {
    print("Не вдалося обчислити дату")
}

dateComponents.day = -5  // Вибираємо 5 днів у минуле

// Обчислюємо дату в минулому
if let fiveDaysAgo = Calendar.current.date(byAdding: dateComponents, to: now) {
    print("П'ять днів до сьогодні: \(fiveDaysAgo)")
} else {
    print("Не вдалося обчислити дату")
}
```

Прикладний вивід:
```
Десять днів після сьогодні: ... // відповідна дата та час
П'ять днів до сьогодні: ... // відповідна дата та час
```

## Поглиблений аналіз:
В минулому для обчислень з датами часто використовували неефективні алгоритми. Сучасні мови програмування, як Swift, включають потужні календарі та часові API, що дозволяють точні та гнучкі обчислення. 

Інші методи включають:
- У бібліотеці DateComponents можна використовувати різні компоненти, не лише дні.
- DateFormatter дозволяє форматувати вивід дат для кращої читабельності.

При обчисленні дати в майбутньому або минулому маємо пам'ятати про часові пояси та особливості календаря, який ми обираємо. 

## Дивіться також:
- [NSCalendar and NSDateComponents](https://developer.apple.com/documentation/foundation/nscalendar) — документація по класах Apple.
