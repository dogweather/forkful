---
title:                "Письмо тестів"
date:                  2024-02-03T19:31:35.660230-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
