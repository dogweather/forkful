---
title:                "Проверка существования директории"
aliases:
- /ru/swift/checking-if-a-directory-exists.md
date:                  2024-01-28T23:55:30.177136-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
