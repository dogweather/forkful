---
title:                "Swift: Запис до стандартного виведення помилок"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому

Запис до стандартного виведення помилок є важливою частиною програмування, оскільки дозволяє знаходити та виправляти помилки в коді. Це також дає можливість стежити за прогресом виконання програми та допомагає у виявленні проблем в перевірці коду.

## Як

Щоб записати до стандартного виведення помилок в Swift, використовуйте ```Swift printError () ``` функцію разом з повідомленням, яке ви хочете вивести. Наприклад:

```Swift
func divide(number: Int, by divisor: Int) throws {
    guard divisor != 0 else {
        throw Error.divisorIsZero
    }
    printError("Ділення на нуль не дозволяється")
}
```

В результаті виконання цього коду, ви отримаєте повідомлення про помилку, якщо програма зустріне ділення на нуль.

## Поглиблене вивчення

Клас Error є одним з найбільш корисних класів у Swift, оскільки він дозволяє перехоплювати та обробляти помилки в коді. Використання printError () функції допомагає зробити цей процес ще більш ефективним та зручним.

## Дивіться також

- [Swift Documentation on Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Using printError () to Debug Your Code in Swift - a Tutorial](https://www.raywenderlich.com/4511414-how-to-use-printerror-to-debug-your-code-in-swift)