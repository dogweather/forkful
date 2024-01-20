---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:17:10.385560-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що та Чому?
Отримання поточної дати – це процес зчитування дати та часу, на які встановлено системні годинники пристрою. Програмісти використовують це, аби відслідковувати події, логувати відмітки часу або управляти завданнями та розкладами.

## Як це зробити:
Swift пропонує простий спосіб отримати поточну дату через стандартний клас `Date`. Ось як це зробити:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Приклад виводу:

```
2023-04-12 14:23:36 +0000
```

Якщо вам потрібен рядковий формат, використовуйте `DateFormatter`:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: currentDate)
print(dateString)
```

Приклад виводу:

```
2023-04-12 14:23:36
```

## Глибше занурення
`Date` у Swift — це структура, що представляє конкретний момент у часі незалежно від часового поясу. Для роботи з часом і датами альтернатив є багато – `Calendar`, `DateComponents`, часові конверсії через `TimeZone` та інші.

Історично `Date` надає цілісність при маніпулюванні часом, проте важливо пам'ятати про часові пояси при роботі з датами, особливо, коли мова йде про міжнародні застосунки. `DateComponents` дозволяє ізолювати різні частини дати, як-то рік, місяць чи день.

Якщо потрібно виконувати операції з датами, типу додання днів або обчислення різниці, використовують `Calendar`. Для упорядкування дати у різних форматах необхідний `DateFormatter`.

## Додатково
Рекомендуємо ознайомитись з наступними ресурсами для розширеного розуміння роботи з датами і часом у Swift:

- [DateFormatter Documentation](https://developer.apple.com/documentation/foundation/dateformatter) (англ.)
- [NSHipster on Date, DateComponents, and DateFormatter](https://nshipster.com/datecomponents/) (англ.)