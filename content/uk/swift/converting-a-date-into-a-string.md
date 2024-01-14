---
title:                "Swift: Перетворення дати в рядок"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому
Навіщо конвертувати дату у рядок? Існує багато ситуацій, коли потрібно вивести дату у певному форматі, наприклад, на екрані або у відправленому повідомленні. Конвертування дати у рядок дозволяє досягти цього одним простим кроком.

## Як
За допомогою функції `dateFormat()` можна конвертувати дату у рядок з заданим форматом. Наприклад, для виведення дати у форматі "дд/мм/рррр" потрібно використати наступний код:
```Swift
let currentDate = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let dateString = formatter.string(from: currentDate)
print(dateString)
```
Результат виведення буде виглядати приблизно так: "25/09/2021".

## Deep Dive
Функція `dateFormat()` дозволяє також використовувати різні символи для форматування дати. Наприклад, символ "E" визначає день тижня в форматі "написання повністю", "EEE" - в форматі "скороченого написання", а "EEEE" - в форматі "назва повністю". Також можна задати часовий пояс за допомогою символу "z", наприклад, "zZZZZ" виведе назву часового поясу відповідно до налаштувань пристрою. Докладніше про всі доступні символи можна дізнатися у [документації Apple](https://developer.apple.com/documentation/foundation/dateformatter).

## Див. також
- [Рядки у Swift](https://developer.apple.com/documentation/swift/string)
- [Процес форматування дати](https://www.hackingwithswift.com/articles/153/how-to-use-dateformat-and-datatimeformatter-in-swift)