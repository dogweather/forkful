---
title:                "Swift: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Причина

Тести написані для того, щоб переконатися, що ваш код працює правильно та уникнути помилок у майбутньому. Це допомагає забезпечити надійність та якість вашої програми.

## Як

Нижче наведений приклад коду Swift для написання простого тесту:

```Swift
import XCTest

class CalculatorTests: XCTestCase {

  func testAdd() {

    let calculator = Calculator()

    let result = calculator.add(2, 3)

    XCTAssertEqual(result, 5)

  }

}
```

У цьому прикладі, ми створюємо екземпляр класу калькулятора, викликаємо метод додавання та перевіряємо, чи отримали ми очікуваний результат. У разі, якщо результат відмінний від очікуваного, тест буде не пройдений та ви отримаєте повідомлення про помилку.

## Глибоке занурення

Тести є важливою частиною розробки програмного забезпечення, оскільки допомагають виявити та виправити помилки ще до випуску програми. Вони також допомагають покращити структуру та якість коду, що сприяє легшій зрозумілості та підтримці програми у майбутньому.

## Дивись також

* [XCTest](https://developer.apple.com/documentation/xctest) - офіційна документація Swift для тестування програм.
* [Swift Testing](https://www.pluralsight.com/courses/swift-testing) - онлайн курс про написання тестів у Swift.
* [Unit Testing in Swift](https://www.raywenderlich.com/709-unit-testing-in-swift-getting-started) - стаття про основи тестування у Swift.