---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:36.562292-07:00
description: "\u041A\u0430\u043A: \u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\
  \u043A\u0441\u0442\u0430 \u0432 \u0444\u0430\u0439\u043B \u043D\u0430 Swift \u043F\
  \u0440\u043E\u0441\u0442\u0430 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043A\u043B\u0430\u0441\u0441\u0430\
  \ `String` \u0438 `FileManager`. \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\
  \u044B\u0439 \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.716594-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u0430\
  \ \u0432 \u0444\u0430\u0439\u043B \u043D\u0430 Swift \u043F\u0440\u043E\u0441\u0442\
  \u0430 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u043A\u043B\u0430\u0441\u0441\u0430 `String` \u0438 `FileManager`."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 24
---

## Как:
Запись текста в файл на Swift проста с использованием класса `String` и `FileManager`. Вот быстрый пример:

```Swift
import Foundation

let stringToWrite = "Привет, Swift!"
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("example.txt")

do {
    try stringToWrite.write(to: fileURL!, atomically: true, encoding: .utf8)
    print("Файл успешно записан")
} catch {
    print("Ошибка записи в файл: \(error)")
}
```

Пример вывода:
```
Файл успешно записан
```

## Глубокое погружение
Запись текстовых файлов существует столько же, сколько и сами компьютеры, часто используется для хранения небольших объемов данных до того, как базы данных стали общепринятыми. Ключевые альтернативы включают базы данных и пользовательские настройки, которые структурированы и более эффективны для больших наборов данных. При записи файлов на Swift метод `write(to:atomically:encoding:)` обеспечивает атомарные записи, предотвращая повреждение данных во время операции записи.

## См. также
- Документация по строкам Swift: https://developer.apple.com/documentation/swift/string
- Руководство по FileManager: https://developer.apple.com/documentation/foundation/filemanager
- Работа с JSON на Swift: https://developer.apple.com/swift/blog/?id=37
- Учебник по работе с файлами на Swift: https://www.raywenderlich.com/1881-file-handling-in-swift-tutorial
