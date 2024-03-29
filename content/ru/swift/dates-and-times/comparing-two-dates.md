---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:44.422718-07:00
description: "\u0421\u0440\u0430\u0432\u043D\u0438\u0432\u0430\u0442\u044C \u0434\u0432\
  \u0435 \u0434\u0430\u0442\u044B \u043F\u043E\u0445\u043E\u0436\u0435 \u043D\u0430\
  \ \u0432\u043E\u043F\u0440\u043E\u0441, \"\u0427\u0442\u043E \u0431\u044B\u043B\u043E\
  \ \u0440\u0430\u043D\u044C\u0448\u0435, \u043A\u0443\u0440\u0438\u0446\u0430 \u0438\
  \u043B\u0438 \u044F\u0439\u0446\u043E?\" \u0442\u043E\u043B\u044C\u043A\u043E \u0441\
  \ \u043A\u0430\u043B\u0435\u043D\u0434\u0430\u0440\u043D\u044B\u043C\u0438 \u0434\
  \u0430\u0442\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u0441\u043E\u0440\u0442\u0438\u0440\u043E\u0432\u043A\u0438\
  \ \u0441\u043E\u0431\u044B\u0442\u0438\u0439,\u2026"
lastmod: '2024-03-13T22:44:45.705855-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0440\u0430\u0432\u043D\u0438\u0432\u0430\u0442\u044C \u0434\u0432\
  \u0435 \u0434\u0430\u0442\u044B \u043F\u043E\u0445\u043E\u0436\u0435 \u043D\u0430\
  \ \u0432\u043E\u043F\u0440\u043E\u0441, \"\u0427\u0442\u043E \u0431\u044B\u043B\u043E\
  \ \u0440\u0430\u043D\u044C\u0448\u0435, \u043A\u0443\u0440\u0438\u0446\u0430 \u0438\
  \u043B\u0438 \u044F\u0439\u0446\u043E?\" \u0442\u043E\u043B\u044C\u043A\u043E \u0441\
  \ \u043A\u0430\u043B\u0435\u043D\u0434\u0430\u0440\u043D\u044B\u043C\u0438 \u0434\
  \u0430\u0442\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u0441\u043E\u0440\u0442\u0438\u0440\u043E\u0432\u043A\u0438\
  \ \u0441\u043E\u0431\u044B\u0442\u0438\u0439,\u2026"
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
---

{{< edit_this_page >}}

## Что и Почему?
Сравнивать две даты похоже на вопрос, "Что было раньше, курица или яйцо?" только с календарными датами. Программисты делают это для сортировки событий, инициации действий и оценки периодов.

## Как:
Swift использует тип `Date` для даты и времени. Вот простой пример сравнения двух дат:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

// Создание двух объектов даты
let date1 = dateFormatter.date(from: "2023/01/01 09:00")!
let date2 = dateFormatter.date(from: "2023/02/01 10:00")!

// Сравнение дат
if date1 == date2 {
    print("Даты одинаковые")
} else if date1 < date2 {
    print("Дата1 раньше Дата2")
} else {
    print("Дата1 позже Дата2")
}
```

Пример вывода:

`Дата1 раньше Дата2`

Операторы сравнения могут использоваться, потому что `Date` соответствует протоколу `Comparable`.

## Глубокое погружение:
Раньше даты не всегда были удобными объектами. Изначально приходилось манипулировать отдельными компонентами, такими как год, месяц и день. Намного хуже. Теперь объекты `Date` в Swift выполняют большую работу, и их сравнение простое благодаря встроенным операторам.

До Swift и `Date` Cocoa Objective-C использовал `NSDate`, но они обратно совместимы, так что старый код все еще может хорошо работать.

И это еще не все, помимо `<`, `>`, и `==` — вы также можете использовать `timeIntervalSince(_:)` для более детального контроля, например:

```Swift
let timeInterval = date2.timeIntervalSince(date1)
```

Это дает вам разницу в секундах. Положительное значение: date2 впереди; отрицательное: позади; ноль: они идентичны. Очень полезно для таймеров, обратных отсчетов и отслеживания продолжительностей. Внутри даты - это просто точки отсчета во времени — думайте о них как о шикарных временных метках.

## Смотрите также:
- Документация Apple о Date: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- Руководство по форматированию дат: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
