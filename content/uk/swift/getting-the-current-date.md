---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що це таке і чому це важливо?

Отримання поточної дати в Swift — це процес зчитування даних про дату та час у реальному часі. Програмісти роблять це для відправки часових позначок, створення функцій зворотного відліку, обробки даних, що залежать від часу.

## Як це робити:

Ось як ви можете отримати поточну дату в Swift:

```Swift
import Foundation

let currentDateTime = Date()
print(currentDateTime)
```

Вивід:

```Swift
2022-03-16T18:31:42Z
```

Код вище повертає поточну дату та час у форматі ISO.

## Погрузимося глибше

Історично, функції для отримання поточної дати відмінно працювали з Unix часом, проте, з розвитком Swift, постала потреба в більш гнучкому і масштабованому способі.

Альтернативний спосіб отримати дату - використати `Calendar` API. Щоб витягнути день, місяць, рік або годину:

```Swift
let date = Date()
let calendar = Calendar.current

let year = calendar.component(.year, from: date)
let month = calendar.component(.month, from: date)
let day = calendar.component(.day, from: date)
```

Дати під керуванням Swift використовують структуру `Date`, що є частиною бібліотеки Foundation. `Date` визначає певний момент в часі, незалежно від часового поясу.

## Варто побачити

- Поглиблена інформація про роботу з датами в Swift: https://developer.apple.com/documentation/foundation/date
- DateFormatter: більше деталей про налаштування формату дати: https://developer.apple.com/documentation/foundation/dateformatter
- Calendar: як розбити дату на дні, місяці, роки та ін.: https://developer.apple.com/documentation/foundation/calendar