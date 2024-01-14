---
title:                "PHP: Написання тестів"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів - це важлива складова процесу програмування. Вони дозволяють перевірити функціональність коду і впевнитись, що він працює правильно. Також тести зберігають команду від недоліків і допомагають знижувати кількість багів у програмі.

## Як

```PHP
// Наприклад, ми маємо функцію, що обчислює суму двох чисел
function add_numbers($a, $b) {
  return $a + $b;
}

// Створюємо тест для перевірки правильності обчислення суми чисел
function test_add_numbers() {
  $expected_result = 10;
  $actual_result = add_numbers(4, 6);
  
  // Перевіряємо, чи отриманий результат співпадає з очікуваним
  if ($actual_result == $expected_result) {
    echo "Test passed!";
  } else {
    echo "Test failed!";
  }
}

// Викликаємо функцію тесту
test_add_numbers();

// Виведе: Test passed!
```

## Детальний огляд

Написання тестів допомагає знижувати кількість помилок у програмі і робить її більш стабільною. Тести також допомагають при внесенні змін до коду, оскільки можна швидко перевірити, чи все ще працює правильно. Також вони допомагають писати більш чистий і понятний код, оскільки тестування вимагає розуміння функціональності кожної частини програми.

## Дивіться також

- [PHPUnit](https://phpunit.de/)
- [Codeception](https://codeception.com/)
- [Чому тести важливі для проектів на PHP](https://devstylelabs.com/uk/cemu-)yang-tests-vazhlivi-dlya-proktiv-na-php/)