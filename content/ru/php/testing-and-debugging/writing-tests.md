---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:02.594220-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041C\u044B \u0440\u0430\u0441\u0441\u043C\u043E\u0442\u0440\u0438\
  \u043C PHPUnit, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\u044E \u0440\
  \u0430\u043C\u043E\u0447\u043D\u0443\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0443 \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\
  \ \u0434\u043B\u044F PHP. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\
  \u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0435\u0433\u043E \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E Composer."
lastmod: '2024-03-13T22:44:45.217145-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u044B \u0440\u0430\u0441\u0441\u043C\u043E\u0442\u0440\u0438\u043C\
  \ PHPUnit, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\u044E \u0440\u0430\
  \u043C\u043E\u0447\u043D\u0443\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0443 \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\
  \ \u0434\u043B\u044F PHP."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
