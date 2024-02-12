---
title:                "Обробка помилок"
date:                  2024-01-26T00:58:21.461314-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/handling-errors.md"
---

{{< edit_this_page >}}

## Що та чому?
Обробка помилок у Swift означає передбачення та реагування на проблеми, які виникають під час виконання вашого коду. Ми робимо це, щоб контролювати хаос — запобігаючи збоям додатків та забезпечуючи користувачу плавну роботу.

## Як це робити:
Swift використовує обробку помилок з блоками `do`, `try` та `catch`. Давайте розглянемо:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Уявімо, що у нас є деяка логіка тут для перевірки, чи файл існує та чи маємо право його читати
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Зміст файлу йде сюди"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Ой! Файл не знайдено.")
} catch FileError.noPermission {
    print("Ах! Немає доступу для читання файлу.")
} catch {
    print("Сталася невідома помилка.")
}

```

Приклад виводу:

```
Ой! Файл не знайдено.
```

## Поглиблений розгляд
Обробка помилок не завжди була такою крутою, як зараз. У Objective-C ви мали справу з покажчиками на об'єкти NSError, що здавалось незграбним. Тепер у нас є елегантніша система зі Swift-перечисленнями та протоколом `Error`.

`throw` у Swift дозволяє нам сигналізувати про те, що щось пішло не так. Блоки `do` діють як сфери, що свідомі помилок, префікс `try` викликає ризиковані операції, а `catch` впорається з речами, якщо вони підуть не так.

Опціонали — це альтернатива для ситуацій, які не зовсім "помилкові", але все ж можуть мати "ніякого результату". Вони трохи схожі на змінні Шрьодінгера — у них є значення або його немає.

Для дійсного заглиблення, перевірте типи `Result`, які є модними гібридами між звичайним поверненням та шаблонами помилок.

## Дивіться також
- Офіційний посібник по обробці помилок Swift: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Кращі практики обробки помилок Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Просунута обробка помилок у Swift: [Стаття на Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)