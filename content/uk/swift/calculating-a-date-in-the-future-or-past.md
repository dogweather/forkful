---
title:                "Swift: Розрахунок дати у майбутньому або минулому."
simple_title:         "Розрахунок дати у майбутньому або минулому."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

"Why" (Чому): Цікавитеся програмуванням на Swift та хочете навчитися обчислювати дати в майбутньому або минулому.

"How To" (Как):

```Swift
// Обчислення дати в майбутньому
let now = Date() // Поточна дата
let futureDate = Calendar.current.date(byAdding: .month, value: 4, to: now) // Додаємо 4 місяці до поточної дати
print(futureDate!) // Виводимо результат: 2020-07-27 08:00:00 +0000

// Обчислення дати в минулому
let now = Date() // Поточна дата
let pastDate = Calendar.current.date(byAdding: .year, value: -2, to: now) // Віднімаємо 2 роки від поточної дати
print(pastDate!) // Виводимо результат: 2018-03-27 08:00:00 +0000
```

"Deep Dive" (Поглиблене вивчення): Для обчислення дат в майбутньому або минулому можна використовувати різні компоненти календаря (роки, місяці, тижні, дні). Також можна задавати різні значення - від'ємні для минулого та додатні для майбутнього. Крім того, детальнішу інформацію можна знайти в документації про Swift.

See Also (Також дивіться): 

- [Документація по Swift](https://docs.swift.org/swift-book/index.html)
- [Туторіал по обчисленню дат на Swift](https://www.hackingwithswift.com/example-code/system/how-to-add-days-to-a-date-using-calendar-datecomponents)
- [Стаття про роботу з датами на Swift](https://medium.com/flawless-app-stories/dates-in-swift-58ef0a0117a2)