---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:47.449965-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ `DateFormatter` \u0434\u043B\u044F \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\
  \u043E\u0432\u0430\u043D\u0438\u044F \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432\
  \ `Date` \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u044B\u0435 \u0441\u0442\u0440\
  \u043E\u043A\u0438. \u0412\u043E\u0442 \u043A\u0430\u043A."
lastmod: '2024-03-13T22:44:45.704124-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 `DateFormatter`\
  \ \u0434\u043B\u044F \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\
  \u0430\u043D\u0438\u044F \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432 `Date`\
  \ \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u044B\u0435 \u0441\u0442\u0440\u043E\
  \u043A\u0438."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как это сделать:
Swift использует `DateFormatter` для преобразования объектов `Date` в читаемые строки. Вот как:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // Вывод: "2023-04-05 14:20:35" (или текущая дата и время)
```

Измените `dateFormat`, чтобы настроить внешний вид вашей даты:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // Вывод: "Среда, Апр 5, 2023"
```

## Глубокое погружение
До `DateFormatter`, Objective-C и ранние версии Swift использовали `NSDateFormatter`, который по сути является тем же самым, но под другим названием. Ключевой момент — знание ISO 8601, стандарта формата даты. Разработчикам нужно находить баланс между пользовательскими форматами и настройками локали пользователя. Почему? Потому что даты читаются по-разному во всем мире. Например, американцы используют "MM/dd/yyyy", в то время как во многих европейских странах используют "dd/MM/yyyy".

Альтернативы? Конечно. Swift предлагает `ISO8601DateFormatter` для дат в формате ISO 8601 и `DateComponentsFormatter` для строк продолжительности, например, "42 минуты". Вы также можете создать собственный формат с помощью `.formatted()` в Swift 5.5 и выше:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // Вывод будет зависеть от настроек вашей локали
```

Осторожно: Создание пользовательских строк может привести к проблемам с локализацией и коду, склонному к ошибкам. По возможности придерживайтесь форматтеров и стандартов.

## Смотрите также
- [Форматирование даты](https://developer.apple.com/documentation/foundation/dateformatter) - Документация Apple о DateFormatter
