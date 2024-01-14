---
title:    "Swift: Написання тестів"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-tests.md"
---

{{< edit_this_page >}}

##Чому

Написання тестів є важливою частиною розробки програмного забезпечення. Вони дозволяють переконатися в тому, що програма працює, як очікувалось, та запобігають появі багів у майбутньому. Також вони допомагають ефективніше вносити зміни та покращення до коду.

##Як

Щоб написати тести для своєї програми, необхідно знати основні концепції тестування у Swift. Ось кілька прикладів коду та їхній результат:

```Swift
// Приклад тесту на перевірку додавання
func testAddition() {
  let calculator = Calculator()
  let result = calculator.add(num1: 5, num2: 7)
  XCTAssertEqual(result, 12)
}
```
Результат: тест пройшов успішно

```Swift
// Приклад тесту на перевірку віднімання
func testSubtraction() {
  let calculator = Calculator()
  let result = calculator.subtract(num1: 10, num2: 5)
  XCTAssertEqual(result, 5)
}
```
Результат: тест пройшов успішно

```Swift
// Приклад тесту на перевірку ділення на нуль
func testDivisionByZero() {
  let calculator = Calculator()
  let result = calculator.divide(num1: 10, num2: 0)
  XCTAssertNil(result)
}
```
Результат: тест пройшов успішно, тому що в результаті ділення на нуль повертається nil.

##Глибокий погляд

Написання тестів - це більше, ніж просто перевірка правильності роботи програми. Це також єдиний спосіб підтвердити, що зміни, які ви вносите до коду, не руйнують його функціональності. Крім того, написання тестів покращує структуру та читабельність коду, оскільки вимагає докладніших коментарів та розмежування логіки програми.

Для ефективного тестування, важливо створювати тести для кожного блоку коду та використовувати функції, такі як `XCTAssert` для перевірки умов.

##Дивись також

- [Документація Apple для тестування у Swift](https://developer.apple.com/documentation/xctest)
- [Книга "Test-Driven Development by Example" Е.Фримена](https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530)
- [Блог-пост "Цікаві факти про тестування у Swift" від Codeacademy](https://www.codecademy.com/articles/things-you-didnt-know-about-testing-in-swift)