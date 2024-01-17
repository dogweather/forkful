---
title:                "Написання тестів"
html_title:           "Swift: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-tests.md"
---

{{< edit_this_page >}}

Що і для чого?

Написання тестів - це процес, що дозволяє програмістам перевірити, чи працює їх код правильно. Вони роблять це, щоб бути впевненими, що їх програма працюватиме без помилок як при значних, так і при невеликих змінах.

Як?

```Swift
func addTwoNumbers(_ a: Int, _ b: Int) -> Int {
  return a + b
}

// Приклад виклику функції
addTwoNumbers(3, 5)

// Очікуваний результат - 8
```

Глибокий занурення

Написання тестів не є новшим явищем у світі програмування. Вони були запроваджені на початку розвитку комп'ютерних науок, але лише у новіших мовах, таких як Swift, вони стали стандартною практикою. Є кілька альтернатив, наприклад, використання живих тестів або ручного тестування коду, але написання тестів дозволяє ефективніше перевірити велику кількість варіантів та забезпечити надійність програми. Щоб побачити приклади найпопулярніших фреймворків для написання тестів у Swift, перегляньте наш покроковий посібник [тут](https://github.com/Quick/Quick) та [тут](https://github.com/Quick/Nimble).

Дивіться також

Ви можете знайти додаткову інформацію щодо написання тестів у Swift у [документації](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/01-introduction.html) Apple та на [сайті](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial) RayWenderlich.