---
title:                "Отримання поточної дати"
aliases:
- uk/swift/getting-the-current-date.md
date:                  2024-02-03T19:11:45.106445-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати в Swift включає використання класу `Date` для доступу до дати та часу, коли запущено додаток. Програмісти потребують отримання поточної дати з безлічі причин, починаючи від маркування часом подій, виконання обчислень з датами та закінчуючи відображенням дат та часу в інтерфейсі користувача.

## Як:
Фреймворк `Foundation` в Swift надає клас `Date`, що робить отримання поточної дати та часу простим. Ось базовий приклад, як отримати поточну дату:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Це дасте вам щось на кшталт:

```
2023-04-12 07:46:23 +0000
```

Формат виводу відповідає стандарту ISO 8601, використовуючи часовий пояс UTC. Проте, можливо, ви захочете відформатувати цю дату для відображення. Клас `DateFormatter` в Swift прийде на допомогу:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Приклад виводу може бути:

```
12 квітня 2023 р. о 10:46:23
```

Зауважте, що формат виводу буде варіюватися залежно від локалі пристрою, на якому запущено код.

Для проектів, що вимагають більш складної маніпуляції з датами, багато розробників Swift звертаються до сторонніх бібліотек, таких як `SwiftDate`. Ось як ви могли б використати `SwiftDate`, щоб отримати поточну дату в певному часовому поясі та форматі:

Спочатку додайте `SwiftDate` до вашого проекту через SPM, CocoaPods або Carthage. Потім:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Це може вивести:

```
2023-04-12 09:46:23
```

Використовуючи `SwiftDate`, ви можете легко маніпулювати датами та часом для різних часових поясів та локалей, спрощуючи складні завдання з обробки дат у ваших Swift додатках.
