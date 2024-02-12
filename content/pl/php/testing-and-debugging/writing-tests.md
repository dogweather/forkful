---
title:                "Pisanie testów"
aliases:
- /pl/php/writing-tests.md
date:                  2024-02-03T19:31:32.896705-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów w programowaniu polega na tworzeniu i uruchamianiu skryptów, które weryfikują, czy kod zachowuje się zgodnie z oczekiwaniami w różnych warunkach. Programiści robią to, aby zapewnić jakość, zapobiec regresji i ułatwić bezpieczny refaktoring, co jest kluczowe dla utrzymania zdrowej, skalowalnej i wolnej od błędów bazy kodu.

## Jak to zrobić:
### Natywny PHP – PHPUnit
Szeroko używanym narzędziem do testowania w PHP jest PHPUnit. Zainstaluj go za pomocą Composera:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Pisanie prostego testu:
Utwórz plik `CalculatorTest.php` w katalogu `tests`:
```php
use PHPUnit\Framework\TestCase;

// Zakładając, że masz klasę Calculator, która dodaje liczby
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Uruchom testy za pomocą:
```bash
./vendor/bin/phpunit tests
```

#### Przykładowe wyjście:
```
PHPUnit 9.5.10 autorstwa Sebastiana Bergmanna i współpracowników.

.                                                                   1 / 1 (100%)

Czas: 00:00.005, Pamięć: 6.00 MB

OK (1 test, 1 twierdzenie)
```

### Biblioteki firm trzecich – Mockery
Do skomplikowanego testowania, w tym mockowania obiektów, popularnym wyborem jest Mockery.

```bash
composer require --dev mockery/mockery
```

#### Integracja Mockery z PHPUnit:
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
        $externalServiceMock->shouldReceive('process')->once()->andReturn('przekształcony wynik');

        $service = new Service($externalServiceMock);
        $wynik = $service->execute();

        $this->assertEquals('przekształcony wynik', $wynik);
    }
}
```
Do uruchomienia użyj tego samego polecenia PHPUnit co powyżej. Mockery pozwala na ekspresyjne i elastyczne obiekty mock, ułatwiając testowanie skomplikowanych interakcji w ramach aplikacji.
