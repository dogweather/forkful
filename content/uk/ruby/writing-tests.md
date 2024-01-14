---
title:                "Ruby: Написання тестів"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є невід'ємною частиною процесу розробки програмного забезпечення, яка допомагає зменшити кількість виявлених помилок та збільшити ефективність роботи команди. В цій статті ми розглянемо, чому важливо писати тести для програм на Ruby.

## Як

Для написання тестів для програм на Ruby потрібно використовувати різноманітні допоміжні бібліотеки, такі як Minitest, RSpec або Cucumber. Давайте розглянемо приклади використання Minitest для тестування функцій звичайного калькулятора.

```Ruby
require 'minitest/autorun'

# створюємо клас Calculator для тестування
class Calculator
  def add(x, y)
    x + y
  end

  def subtract(x, y)
    x - y
  end
end

# створюємо тестовий клас CalculatorTest
class CalculatorTest < Minitest::Test
  def setup # метод, який викликається перед кожним тестом
    @calculator = Calculator.new # створюємо екземпляр класу Calculator
  end

  def test_add # метод для тестування функції add
    assert_equal 4, @calculator.add(2, 2) # перевіряємо, чи повертає функція правильне значення
  end

  def test_subtract # метод для тестування функції subtract
    assert_equal 2, @calculator.subtract(4, 2) # перевіряємо, чи повертає функція правильне значення
  end
end
```

Виконання цих тестів дає нам впевненість, що функції калькулятора працюють правильно. Це дозволяє нам швидко знаходити та виправляти будь-які проблеми з кодом.

## Deep Dive

Написання тестів дає розробникам впевненість у функціональності свого коду та надає можливість змінювати чи додавати новий код, не турбуючись про його вплив на вже наявну функціональність. Крім того, це також допомагає зменшити час налагодження та покращити якість програми в цілому.

Написання тестів також допомагає виробити гарну практику розбиття програм на менші частини та дотримання принципів SOLID (Single Responsibility, Open/Closed, Liskov Substitution, Interface Segregation, Dependency Inversion).

## Дивіться також

- [Документація по Minitest](https://github.com/seattlerb/minitest)
- [Документація по RSpec](https://rspec.info/)
- [Документація по Cucumber](https://cucumber.io/)