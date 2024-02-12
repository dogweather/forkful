---
title:                "Письмо тестів"
aliases: - /uk/swift/writing-tests.md
date:                  2024-02-03T19:32:37.176420-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Написання тестів на Swift полягає у створенні та виконанні коду, що перевіряє коректність інших кодових одиниць у вашому застосунку. Програмісти роблять це, щоб забезпечити надійність, виявити помилки на ранніх стадіях розробки, та полегшити майбутній рефакторинг коду без непередбачених наслідків.

## Як:
Swift підтримує тестування через свій фреймворк XCTest, який інтегрований у Xcode. Ви можете написати модульні тести, щоб перевірити окремі частини вашого коду, наприклад, функцію, яка розраховує суму двох чисел.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "Функція суми не повернула очікуване значення.")
    }
}
```

Щоб запустити цей тест, зазвичай ви натискаєте Command-U у Xcode. Вивід у навігаторі тестів Xcode покаже, чи тест пройшов успішно чи ні.

Наприклад, вивід успішного тесту:
```
Test Case '-[YourAppTests testSum]' passed (0.005 секунд).
```

Для більш розширених сценаріїв тестування ви можете використовувати сторонні бібліотеки, такі як Quick/Nimble, які пропонуют більш виразний синтаксис для написання тестів.

З Quick/Nimble, ви могли б написати той самий тест так:

```swift
// Додайте Quick та Nimble до вашого Swift package manager або використовуйте CocoaPods/Carthage для їх встановлення
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Калькулятор") {
            context("при сумуванні чисел") {
                it("повинен повертати коректну суму") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Запуск цього тесту дасть вам подібний вивід у вашу консоль тестування чи журнал інструменту CI/CD, вказуючи, чи тест успішний чи ні, з більш зрозумілим форматом для опису тестів та очікувань.
