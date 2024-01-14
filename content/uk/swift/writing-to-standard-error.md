---
title:    "Swift: Писання у стандартний помилковий потік"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому: Чому люди пишуть у стандартну помилку

Запис до стандартного виводу помилки використовується тоді, коли ви хочете зробити нагадування, що щось пішло не так у вашій програмі. Це допомагає легше відстежувати та виправляти помилки у вашому коді.

## Як: Приклади коду та виведення до стандартної помилки

Для виводу до стандартної помилки використовується команда `print()` з параметром `stderr`. Наприклад:
```Swift
print("Це приклад виводу до стандартної помилки", to: &stderr)
```
Ви можете також створити свою власну функцію для виводу до стандартної помилки, зокрема з використанням оператора `guard`, який дозволяє перевірити певні умови та вивести до стандартної помилки у разі їх невиконання. Наприклад:
```Swift
func printToStderr(_ message: String) {
    guard !message.isEmpty else {
        print("Меседж не може бути порожнім", to: &stderr)
        return
    }
    print(message, to: &stderr)
}
// Використання функції:
printToStderr("Це демонстрація виводу до стандартної помилки")
```

### Виведення помилки до стандартного виводу

Якщо ви хочете вивести помилку до стандартного виводу, використовуйте команду `exit(1)` у заголовку вашої програми. Це дозволить вивести текст помилки та закрити програму з відповідним кодом помилки.

## Глибока занурення: Детальна інформація про виведення до стандартної помилки

Також варто зазначити, що виведення до стандартного виводу може бути корисним при виконанні великих програм, де необхідно відстежувати помилки та висловлювати попередження користувачеві.

Для того, щоб вивести до стандартної помилки у більшій кількості місць у вашому коді, ви можете створити окрему функцію або розширення для класу `String` з методом `printToStderr()`. Це допоможе зменшити кількість дубльованого коду та полегшить відстеження помилок у вашому програмному продукті.

## Дивіться також

- [Документація по виведенню до стандартної помилки в Swift](https://developer.apple.com/documentation/foundation/standarderror)
- [Відео про використання `guard` та `exit` у Swift](https://www.youtube.com/watch?v=lE9bKAwdoZ4)
- [Стаття про створення розширень в Swift](https://medium.com/@mimicatcodes/create-swift-extensions-for-commonly-used-methods-e457b2f70574)