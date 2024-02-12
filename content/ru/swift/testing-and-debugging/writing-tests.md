---
title:                "Написание тестов"
aliases:
- /ru/swift/writing-tests.md
date:                  2024-01-29T00:05:42.077928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов - это создание кода, который проверяет, работает ли ваше программное обеспечение как запланировано. Программисты тестируют для того, чтобы замечать ошибки на ранних стадиях, обеспечивать качество и упрощать обслуживание.

## Как:
Swift использует фреймворк XCTest для тестирования. Вот простой тест для функции `add(a:b:)`:

```Swift
import XCTest

class MathTests: XCTestCase {

    func testAdd() {
        let result = add(a: 2, b: 3)
        XCTAssertEqual(result, 5, "Ожидалось, что 2 + 3 будет равно 5")
    }

    func add(a: Int, b: Int) -> Int {
        return a + b
    }
}
```
Запускайте тесты с помощью Навигатора Тестов в Xcode или используйте `cmd+U`. Вывод должен гласить:

```plaintext
Test Suite 'Все тесты' пройдены на ...
    Выполнено 1 тест, с 0 неудачами (0 неожиданными) за 0.001 (0.004) секунд
```

## Погружение
XCTest, являющийся частью Xcode с 2013 года, пришел на смену OCUnit. Альтернативы - это Quick (фреймворк BDD) и SnapshotTesting (тесты UI). Реализация тестирования опирается на функции утверждений, тестовые случаи и, при необходимости, наборы тестов, используя возможности фреймворка XCTest.

## Смотрите также
- [Обзор XCTest от Apple](https://developer.apple.com/documentation/xctest)
- [Руководство по модульному тестированию и тестированию UI от Ray Wenderlich для iOS](https://www.raywenderlich.com/21020457-ios-unit-testing-and-ui-testing-tutorial)
- [Тестирование кода Swift с помощью Quick](https://github.com/Quick/Quick)
- [SnapshotTesting на GitHub](https://github.com/pointfreeco/swift-snapshot-testing)
