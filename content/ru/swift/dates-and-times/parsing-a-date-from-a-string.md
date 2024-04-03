---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:09.326793-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\
  \u043E\u0440 \u0434\u0430\u0442 \u0434\u043E\u0441\u0442\u0430\u0442\u043E\u0447\
  \u043D\u043E \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E `DateFormatter`. \u0412\u043E\u0442 \u043A\u043E\u0440\u043E\
  \u0442\u043A\u0438\u0439 \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.700639-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\
  \u0440 \u0434\u0430\u0442 \u0434\u043E\u0441\u0442\u0430\u0442\u043E\u0447\u043D\
  \u043E \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E `DateFormatter`."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как это сделать:
Swift делает разбор дат достаточно простым с помощью `DateFormatter`. Вот короткий пример:

```Swift
import Foundation

let dateString = "2023-04-01"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

if let parsedDate = dateFormatter.date(from: dateString) {
    print("Разобранная дата: \(parsedDate)")
} else {
    print("Не удалось разобрать дату.")
}
```

Пример вывода может выглядеть так, в зависимости от вашего часового пояса:

```
Разобранная дата: 2023-03-31 22:00:00 +0000
```

Помните, вывод по умолчанию в UTC!

## Глубокое погружение
Еще со времен Objective-C, разработчики iOS использовали `NSDateFormatter`, который был перенесен в Swift как `DateFormatter`. Ранее работа с датами была довольно сложной из-за различий в форматах и часовых поясах. К счастью, `DateFormatter` в Swift стандартизирует этот процесс.

Хотя `DateFormatter` подходит для обычных сценариев, существуют альтернативы, такие как `ISO8601DateFormatter`, для форматов ISO 8601, и вы даже можете погрузиться в более низкоуровневый API `Cocoa` с использованием `CFDateFormatter` для более тонкой настройки.

При реализации разбора даты всегда устанавливайте `locale` как `posix` (`en_US_POSIX`), чтобы избежать неожиданного поведения из-за пользовательских настроек. Также имейте в виду, что разбор дат требует много ресурсов, поэтому повторно используйте ваш форматтер или рассмотрите возможность использования `DateComponents` для повторяющихся задач.

## Смотрите также
- [NSDateFormatter - Apple Developer](https://developer.apple.com/documentation/foundation/nsdateformatter)
