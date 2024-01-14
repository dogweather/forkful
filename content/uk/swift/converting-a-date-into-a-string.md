---
title:    "Swift: Перетворення дати у рядок"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому
Конвертація дати в рядок є важливою функцією для багатьох програмістів, оскільки дати часто використовуються для відстеження та розподілу даних. В цій статті ми розглянемо, як конвертувати дату в рядок за допомогою Swift.

## Як це зробити
```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let dateString = dateFormatter.string(from: date)
print(dateString)

// Output: 22/09/2021
```

Цей приклад показує, як використовувати `DateFormatter` для зміни формату дати. У цьому випадку ми змінюємо формат на `dd/MM/yyyy`, але можливі інші опції форматування, залежно від потреб.

## Глибше погляд
Для конвертації дати в рядок у Swift використовується утиліта `DateFormatter`. Це дозволяє змінити формат дати, а також вказати різні параметри, такі як часовий пояс або мова.

## Дивіться також
- [Робота з датами в Swift](https://developer.apple.com/documentation/foundation/datecomponents)
- [Створення власного формату дати за допомогою DateFormatter](https://learnappmaking.com/dateformatter-swift-how-to/)
- [Оновлений варіант конвертації дати у рядок для iOS 10+](https://sarunw.com/posts/how-to-convert-a-string-to-date/)