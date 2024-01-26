---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
Тестування коду – це процес перевірки, що ваш код працює, як задумано. Програмісти тестують, щоб запобігти помилкам, забезпечити якість та спростити майбутнє оновлення коду.

## Як це робити:
У PHP для написання тестів часто використовують бібліотеку PHPUnit. Давайте створимо простий тест.

```PHP
<?php
use PHPUnit\Framework\TestCase;

class SampleTest extends TestCase
{
    public function testTrueAssertsToTrue()
    {
        $this->assertTrue(true);
    }
}
```

Команда для запуску тесту:

```bash
./vendor/bin/phpunit --filter SampleTest
```

Запуск видасть такий результат:

```
OK (1 test, 1 assertion)
```

## Глибоке занурення
Тестування в PHP йде ще з початку 2000-х, з PHPUnit як де-факто стандартом. Є альтернативи: PHPSpec, Behat, Codeception. При написанні тестів використовують принципи TDD (Test-Driven Development), де спочатку створюють тест, а вже потім – код.

## Більше інформації
- [Офіційна документація PHPUnit](https://phpunit.de/)
- [PHPSpec](http://www.phpspec.net/en/stable/)
- [Behat](https://docs.behat.org/en/latest/)
- [Codeception](https://codeception.com/)
