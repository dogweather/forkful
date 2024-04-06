---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:35.660230-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\u043E\u043F\
  \u0443\u043B\u044F\u0440\u043D\u0438\u043C \u0456\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\
  \u0432\u0430\u043D\u043D\u044F \u0432 PHP \u0454 PHPUnit. \u0412\u0441\u0442\u0430\
  \u043D\u043E\u0432\u0456\u0442\u044C \u0439\u043E\u0433\u043E \u0447\u0435\u0440\
  \u0435\u0437 Composer."
lastmod: '2024-03-13T22:44:49.436198-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C \u0456\u043D\
  \u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F\
  \ \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F \u0432 PHP \u0454\
  \ PHPUnit."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

## Як робити:


### Рідний PHP – PHPUnit
Популярним інструментом для тестування в PHP є PHPUnit. Встановіть його через Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Написання простого тесту:
Створіть файл `CalculatorTest.php` у директорії `tests`:
```php
use PHPUnit\Framework\TestCase;

// Припустимо, у вас є клас Calculator, який додає числа
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Запустіть тести за допомогою:
```bash
./vendor/bin/phpunit tests
```

#### Приклад виводу:
```
PHPUnit 9.5.10 від Sebastian Bergmann та співавторів.

.                                                                   1 / 1 (100%)

Час: 00:00.005, Пам'ять: 6.00 MB

OK (1 тест, 1 справдження)
```

### Сторонні бібліотеки – Mockery
Для складних тестувань, включно з макетуванням об'єктів, популярним вибором є Mockery.

```bash
composer require --dev mockery/mockery
```

#### Інтеграція Mockery з PHPUnit:
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
Для запуску використовуйте ту саму команду PHPUnit, що й вище. Mockery дозволяє з легкістю створювати виразні та гнучкі макети об'єктів, полегшуючи тестування складних взаємодій у вашому застосунку.
