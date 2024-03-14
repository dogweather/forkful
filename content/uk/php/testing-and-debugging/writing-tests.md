---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:35.660230-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\
  \u0430\u043D\u043D\u0456 \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\
  \u043F\u0443\u0441\u043A \u0441\u043A\u0440\u0438\u043F\u0442\u0456\u0432, \u044F\
  \u043A\u0456 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C\
  , \u0449\u043E \u043A\u043E\u0434 \u043F\u0440\u0430\u0446\u044E\u0454 \u0442\u0430\
  \u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0454\u0442\u044C\u0441\u044F\
  , \u0432 \u0440\u0456\u0437\u043D\u0438\u0445 \u0443\u043C\u043E\u0432\u0430\u0445\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.436198-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\
  \u0430\u043D\u043D\u0456 \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\
  \u043F\u0443\u0441\u043A \u0441\u043A\u0440\u0438\u043F\u0442\u0456\u0432, \u044F\
  \u043A\u0456 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C\
  , \u0449\u043E \u043A\u043E\u0434 \u043F\u0440\u0430\u0446\u044E\u0454 \u0442\u0430\
  \u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0454\u0442\u044C\u0441\u044F\
  , \u0432 \u0440\u0456\u0437\u043D\u0438\u0445 \u0443\u043C\u043E\u0432\u0430\u0445\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
---

{{< edit_this_page >}}

## Що та Чому?
Написання тестів у програмуванні передбачає створення та запуск скриптів, які перевіряють, що код працює так, як очікується, в різних умовах. Програмісти роблять це, щоб забезпечити якість, запобігти регресам і полегшити безпечний рефакторинг, що є важливим для підтримки здорового, масштабованого та безбагового коду.

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
