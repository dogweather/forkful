---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:30.177136-07:00
description: "\u0412 Swift, \u043F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u043D\
  \u0430\u043B\u0438\u0447\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\
  \u0438\u0438 \u043F\u043E\u043C\u043E\u0433\u0430\u0435\u0442 \u0432\u0430\u043C\
  \ \u043F\u043E\u0434\u0442\u0432\u0435\u0440\u0434\u0438\u0442\u044C \u0441\u043E\
  \u0441\u0442\u043E\u044F\u043D\u0438\u0435 \u0444\u0430\u0439\u043B\u043E\u0432\u043E\
  \u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u044B \u043F\u0435\u0440\u0435\u0434\
  \ \u0442\u0435\u043C, \u043A\u0430\u043A \u0447\u0438\u0442\u0430\u0442\u044C \u0438\
  \u043B\u0438 \u0437\u0430\u043F\u0438\u0441\u044B\u0432\u0430\u0442\u044C \u0434\
  \u0430\u043D\u043D\u044B\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\u2026"
lastmod: '2024-03-13T22:44:45.709427-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Swift, \u043F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u043D\u0430\
  \u043B\u0438\u0447\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0438\
  \u0438 \u043F\u043E\u043C\u043E\u0433\u0430\u0435\u0442 \u0432\u0430\u043C \u043F\
  \u043E\u0434\u0442\u0432\u0435\u0440\u0434\u0438\u0442\u044C \u0441\u043E\u0441\u0442\
  \u043E\u044F\u043D\u0438\u0435 \u0444\u0430\u0439\u043B\u043E\u0432\u043E\u0439\
  \ \u0441\u0438\u0441\u0442\u0435\u043C\u044B \u043F\u0435\u0440\u0435\u0434 \u0442\
  \u0435\u043C, \u043A\u0430\u043A \u0447\u0438\u0442\u0430\u0442\u044C \u0438\u043B\
  \u0438 \u0437\u0430\u043F\u0438\u0441\u044B\u0432\u0430\u0442\u044C \u0434\u0430\
  \u043D\u043D\u044B\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\u2026"
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
---

{{< edit_this_page >}}

## Что и Почему?
В Swift, проверка наличия директории помогает вам подтвердить состояние файловой системы перед тем, как читать или записывать данные. Программисты делают это, чтобы избежать ошибок, таких как чтение из несуществующей директории, что может привести к сбою приложения или к некорректным операциям.

## Как это сделать:
В Swift за это отвечает `FileManager`. Используйте его метод `fileExists(atPath:)`:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path) {
    print("Ага, она есть!")
} else {
    print("Нет, не существует.")
}
```

Пример вывода, если директория существует:

```
Ага, она есть!
```

Или, если её нет:

```
Нет, не существует.
```

## Глубокое погружение
До появления `FileManager`, который был введен с фреймворком Foundation, для проверки путей обычно использовались UNIX команды в скриптах. Но `FileManager` проще и безопаснее. Альтернативы в Swift включают работу с классом `URL` и его методом `checkResourceIsReachable()`, хотя он больше подходит для проверки доступности файла и может генерировать ошибки. Внутренне, `FileManager` использует системный вызов `stat` для проверки существования пути без учета того, является ли это файлом или директорией, так что когда вам нужно различать это, придется дополнительно изучить атрибуты пути.

## Смотрите также
- Документация Swift: [`FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- Книга Swift: [Работа с директориями](https://docs.swift.org/swift-book/)
- Форумы разработчиков Apple: [Доступ к файловой системе](https://developer.apple.com/forums/tags/file-system/)
