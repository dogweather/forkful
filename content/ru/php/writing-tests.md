---
title:                "Написание тестов"
date:                  2024-01-29T00:06:02.594220-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Тестирование проверяет, выполняет ли ваш код то, что от него требуется. Это экономит время, выявляя ошибки на раннем этапе, и гарантирует, что изменения в коде не нарушат работу.

## Как это сделать:
Мы рассмотрим PHPUnit, популярную рамочную программу тестирования для PHP. Сначала установите его с помощью Composer:

```bash
composer require --dev phpunit/phpunit
```

Теперь давайте напишем простой тест. Представьте, что у вас есть класс `Calculator` с методом `add`.

```php
// Calculator.php
class Calculator {
    public function add($a, $b) {
        return $a + $b;
    }
}
```

Вот как вы можете его протестировать:

```php
// CalculatorTest.php
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase {
    public function testAddition() {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```

Запустите тест с помощью:

```bash
./vendor/bin/phpunit CalculatorTest
```

Вывод покажет, пройдены тесты или нет.

## Погружение в тему
Тестирование не всегда было важной частью раработки в PHP. Изначально многие просто сочиняли код и вручную проверяли его работоспособность. Теперь тестирование - это закон. PHPUnit начал набирать популярность в 2000-х, и теперь это практически стандарт. Альтернативы? Конечно, есть PHPSpec и Behat для начала. Под капотом PHPUnit использует утверждения для сравнения ожидаемых и фактических результатов и тестовые двойники (моки, стабы, шпионы) для имитации внешних зависимостей.

## Смотрите также
- Руководство по PHPUnit: https://phpunit.de/manual/current/en/index.html
- PHP Правильный путь (Тестирование): http://www.phptherightway.com/#testing
- Mockery (фреймворк мокирования для PHPUnit): http://docs.mockery.io/en/latest/
