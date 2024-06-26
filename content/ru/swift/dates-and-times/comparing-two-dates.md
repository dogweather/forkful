---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:44.422718-07:00
description: "\u041A\u0430\u043A: Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442 \u0442\u0438\u043F `Date` \u0434\u043B\u044F \u0434\u0430\u0442\
  \u044B \u0438 \u0432\u0440\u0435\u043C\u0435\u043D\u0438. \u0412\u043E\u0442 \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0441\
  \u0440\u0430\u0432\u043D\u0435\u043D\u0438\u044F \u0434\u0432\u0443\u0445 \u0434\
  \u0430\u0442."
lastmod: '2024-03-13T22:44:45.705855-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0442\
  \u0438\u043F `Date` \u0434\u043B\u044F \u0434\u0430\u0442\u044B \u0438 \u0432\u0440\
  \u0435\u043C\u0435\u043D\u0438."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

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
