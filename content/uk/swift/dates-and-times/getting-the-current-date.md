---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:45.106445-07:00
description: "\u042F\u043A: \u0424\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\
  \ `Foundation` \u0432 Swift \u043D\u0430\u0434\u0430\u0454 \u043A\u043B\u0430\u0441\
  \ `Date`, \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u043E\u0442\u0440\u0438\
  \u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457\
  \ \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u0443 \u043F\u0440\u043E\
  \u0441\u0442\u0438\u043C. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\u0432\u0438\
  \u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u044F\u043A \u043E\u0442\u0440\
  \u0438\u043C\u0430\u0442\u0438 \u043F\u043E\u0442\u043E\u0447\u043D\u0443 \u0434\
  \u0430\u0442\u0443."
lastmod: '2024-03-13T22:44:49.937309-06:00'
model: gpt-4-0125-preview
summary: "\u0424\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A `Foundation` \u0432\
  \ Swift \u043D\u0430\u0434\u0430\u0454 \u043A\u043B\u0430\u0441 `Date`, \u0449\u043E\
  \ \u0440\u043E\u0431\u0438\u0442\u044C \u043E\u0442\u0440\u0438\u043C\u0430\u043D\
  \u043D\u044F \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\
  \u0438 \u0442\u0430 \u0447\u0430\u0441\u0443 \u043F\u0440\u043E\u0441\u0442\u0438\
  \u043C."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

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
