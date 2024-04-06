---
date: 2024-01-20 17:32:35.280184-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\u0440\u0438\
  \u043A\u043B\u0430\u0434\u043D\u0438\u0439 \u0432\u0438\u0432\u0456\u0434."
lastmod: '2024-04-05T21:53:50.010949-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

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
