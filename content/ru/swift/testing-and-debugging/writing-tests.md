---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:42.077928-07:00
description: "\u041A\u0430\u043A: Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442 \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A XCTest\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u043D\
  \u0438\u044F. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0442\
  \u0435\u0441\u0442 \u0434\u043B\u044F \u0444\u0443\u043D\u043A\u0446\u0438\u0438\
  \ `add(a:b:)`."
lastmod: '2024-03-13T22:44:45.690244-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0444\
  \u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A XCTest \u0434\u043B\u044F \u0442\
  \u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
