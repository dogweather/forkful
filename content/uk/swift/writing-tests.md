---
title:                "Написання тестів"
date:                  2024-01-19
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Що та Чому?
Тестування коду — це перевірка частин програми на коректність. Програмісти пишуть тести, щоб забезпечити надійність та якість коду, а також щоб уникнути помилок у майбутньому.

## Як?
```Swift
import XCTest
@testable import MyAwesomeApp

class MyTests: XCTestCase {

    func testExample() {
        let result = MyClass().multiplyNumbers(3, 5)
        XCTAssertEqual(result, 15, "Multiplication should be 15")
    }
}

class MyClass {
    func multiplyNumbers(_ a: Int, _ b: Int) -> Int {
        return a * b
    }
}
```
Вихідні дані:
```
Test Suite 'MyTests' passed at 2023-03-21 18:36:24.824.
        Executed 1 test, with 0 failures (0 unexpected) in 0.004 (0.006) seconds
```

## Поглиблений Розділ
Тестування коду має довгу історію і пройшло еволюцію від простих перевірок до комплексних автоматизованих систем. Альтернативами до XCTest у Swift можуть бути Quick/Nimble, або зовнішні фреймворки, як Appium чи Calabash. Глибина і складність тестів може варіюватись від простих unit tests до комплексних UI та integration tests.

## Додатково
- [Swift Testing](https://swift.org/documentation/#the-swift-programming-language)
- [XCTest Documentation](https://developer.apple.com/documentation/xctest)
- [Про тестування від Apple](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/01-introduction.html)
- [Quick/Nimble GitHub](https://github.com/Quick/Nimble)
