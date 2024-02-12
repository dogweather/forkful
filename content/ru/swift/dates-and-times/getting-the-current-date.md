---
title:                "Получение текущей даты"
aliases: - /ru/swift/getting-the-current-date.md
date:                  2024-01-28T23:58:48.430346-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
В Swift получение текущей даты включает доступ к текущим настройкам времени и даты системы. Программисты делают это для того, чтобы ставить временные метки событиям, планировать задачи или просто отображать дату и время в своих приложениях.

## Как:
Получение текущей даты и времени в Swift простое:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```
Пример вывода:
```
2023-04-10 16:20:32 +0000
```
Если вам нужен конкретный формат:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let formattedDate = formatter.string(from: Date())
print(formattedDate)
```
Пример вывода:
```
2023-04-10 16:20:32
```

## Глубокое Погружение
Структура `Date` в Swift является частью фреймворка Foundation, который пришел из `NSDate` в Objective-C. Со временем Swift предложил современный подход с `Date`, который более выразителен и безопасен.

Существуют альтернативы `Date()` для получения текущего времени. Например, `NSDate()`, который примерно тот же, но менее удобен для Swift, и более низкоуровневые API, такие как `gettimeofday()`, для получения более точного системного времени. Но `Date()` является основным выбором для большинства разработчиков Swift, поскольку он сочетает в себе простоту использования с достаточной точностью для типичных случаев использования.

`Date()` в Swift получает системное время, которое часто бывает в скоординированном универсальном времени (UTC). Поэтому при его прямом выводе без формата оно появляется с UTC-сдвигом. Вот почему форматировщики так популярны; они позволяют настроить дату и время для любого указанного часового пояса и формата, делая его удобным для восприятия при отображении. Реализация собственных корректировок часового пояса без форматировщиков возможна, но это переизобретение колеса и подвержено ошибкам из-за изменений летнего времени и високосных секунд.

## Смотрите также
- Официальная документация Apple по `Date`: [Date - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- Руководство по DateFormatter: [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- Для более глубоких знаний о дате и времени в компьютерных системах, смотрите видео Computerphile на YouTube [Computerphile’s video on YouTube](https://www.youtube.com/watch?v=-5wpm-gesOY).
