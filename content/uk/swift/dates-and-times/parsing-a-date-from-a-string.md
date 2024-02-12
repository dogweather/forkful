---
title:                "Розбір дати з рядка"
aliases:
- uk/swift/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:57.343170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Парсинг дати з рядка полягає в конвертації текстових представлень дати та часу у об’єкт `Date`. Цей процес є невід’ємним в додатках, де дати передаються у вигляді рядків, наприклад, у відповідях API або введеннях користувачів, дозволяючи легше маніпулювати датами та форматувати їх.

## Як це зробити:

### Використання `DateFormatter` з Foundation
Стандартна бібліотека Swift, Foundation, надає `DateFormatter` для конвертації рядків у об’єкти `Date` і навпаки. Щоб проаналізувати дату з рядка, ви вказуєте формат дати, який відповідає рядку, потім використовуєте форматер для її парсингу.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Дата проаналізована: \(date)")
} else {
    print("Не вдалося проаналізувати дату")
}
// Приклад виводу: Дата проаналізована: 2023-04-29 22:00:00 +0000
```

Зауважте, що вивід може варіюватися залежно від вашого часового поясу.

### Використання `ISO8601DateFormatter`
Для форматів дати ISO 8601, Swift надає спеціалізований форматер, `ISO8601DateFormatter`, який спрощує процес парсингу.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Проаналізована ISO8601 дата: \(date)")
} else {
    print("Не вдалося проаналізувати ISO8601 дату")
}
// Приклад виводу: Проаналізована ISO8601 дата: 2023-04-30 15:00:00 +0000
```

### Використання сторонньої бібліотеки: SwiftDate
Хоча Swift надає надійні інструменти для парсингу дат, сторонні бібліотеки, як-от SwiftDate, пропонують ще більше гнучкості та зручності. Після додавання SwiftDate до вашого проєкту, парсинг стає настільки ж простим, як:

```swift
import SwiftDate

let dateString = "30 квітня 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Дата проаналізована з SwiftDate: \(date)")
} else {
    print("Не вдалося проаналізувати дату з SwiftDate")
}
// Приклад виводу: Дата проаналізована з SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate спрощує парсинг за допомогою природньої мови та широкого спектру форматів дат, стаючи потужним доповненням до вашої набору інструментів програмування на Swift.
