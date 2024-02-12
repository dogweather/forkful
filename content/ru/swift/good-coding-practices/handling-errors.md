---
title:                "Обработка ошибок"
aliases: - /ru/swift/handling-errors.md
date:                  2024-01-28T23:58:50.619375-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Обработка ошибок в Swift означает предвидение и реагирование на проблемы, возникающие при выполнении вашего кода. Мы делаем это для того, чтобы контролировать хаос — предотвращать аварийное завершение работы приложений и обеспечивать плавный опыт пользователя.

## Как:
Swift использует обработку ошибок с помощью блоков `do`, `try` и `catch`. Давайте посмотрим:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Представим, что у нас есть некоторая логика здесь, чтобы проверить, существует ли файл и имеем ли мы разрешение на его чтение
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Содержимое файла идет здесь"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Ой! Файл не найден.")
} catch FileError.noPermission {
    print("Ах! Нет разрешения на чтение файла.")
} catch {
    print("Произошла неизвестная ошибка.")
}

```

Пример вывода:

```
Ой! Файл не найден.
```

## Глубокое Погружение
Обработка ошибок не всегда была такой изящной, как сейчас. В Objective-C вы имели дело с указателями на объекты NSError, что казалось громоздким. Теперь у нас есть более элегантная система с перечислениями Swift и протоколом `Error`.

`throw` в Swift позволяет нам сигнализировать о том, что что-то пошло не так. Блоки `do` действуют как области, осведомленные об ошибках, префикс `try` вызывает рискованные операции, а `catch` обрабатывает ситуации, если что-то идет не так.

Опционалы являются альтернативой для ситуаций, которые не совсем "статус ошибки", но все же могут иметь "нет результата". Они немного похожи на переменные Шрёдингера — у них есть значение или его нет.

Для действительного погружения рекомендуется ознакомиться с типами `Result`, которые являются стильными гибридами между обычными возвращаемыми значениями и шаблонами ошибок.

## Смотрите также
- Официальное руководство по обработке ошибок Swift: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Лучшие практики обработки ошибок в Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Продвинутая обработка ошибок в Swift: [Статья на Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
