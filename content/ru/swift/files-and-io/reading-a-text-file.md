---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:59.521970-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u043F\u0440\u043E\u0447\u0438\u0442\
  \u0430\u0442\u044C \u0442\u0435\u043A\u0441\u0442 \u0438\u0437 \u0444\u0430\u0439\
  \u043B\u0430 \u0432 Swift, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 \u0443\u0434\u043E\u0431\u043D\u044B\u0435 \u043C\u0435\u0442\u043E\
  \u0434\u044B \u043A\u043B\u0430\u0441\u0441\u0430 `String`. \u0412\u043E\u0442 \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.714834-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043F\u0440\u043E\u0447\u0438\u0442\u0430\
  \u0442\u044C \u0442\u0435\u043A\u0441\u0442 \u0438\u0437 \u0444\u0430\u0439\u043B\
  \u0430 \u0432 Swift, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\
  \u0435 \u0443\u0434\u043E\u0431\u043D\u044B\u0435 \u043C\u0435\u0442\u043E\u0434\
  \u044B \u043A\u043B\u0430\u0441\u0441\u0430 `String`."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как это сделать:
Чтобы прочитать текст из файла в Swift, используйте удобные методы класса `String`. Вот простой пример:

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let content = try String(contentsOfFile: filePath, encoding: .utf8)
        print(content)
    } catch {
        print("Упс! Что-то пошло не так: \(error)")
    }
}
```
Если "example.txt" содержит "Привет, мир!", то вывод будет:
```
Привет, мир!
```

## Подробнее
Чтение текстовых файлов – это старо как мир в мире программирования. В начале шло все от перфокарт и ленты. Теперь, с высокоуровневыми языками вроде Swift, это просто. Приведенный выше фрагмент использует `String(contentsOfFile:)`, но есть альтернативы:

- `FileManager`: Хорош для более сложных файловых операций.
- `InputStream`: Используйте его при работе с большими файлами – менее интенсивно использует память.
- `URLSession`: Используйте для получения файлов с удаленного сервера.

Подход `String(contentsOfFile:)` может быть ресурсоемким, если используется с огромными файлами. Чтобы избежать проблем, рассмотрите методы, основанные на потоках, или чтение частями.

## Смотрите также
Погрузитесь в официальную документацию Swift:
- [String](https://developer.apple.com/documentation/swift/string)
- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Работа с URL Session](https://developer.apple.com/documentation/foundation/url_loading_system/fetching_website_data_into_memory)

Для более глубокого понимания ознакомьтесь с этими ресурсами:
- [Руководство по программированию файловой системы Apple](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
