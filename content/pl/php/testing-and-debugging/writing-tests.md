---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:32.896705-07:00
description: "Pisanie test\xF3w w programowaniu polega na tworzeniu i uruchamianiu\
  \ skrypt\xF3w, kt\xF3re weryfikuj\u0105, czy kod zachowuje si\u0119 zgodnie z oczekiwaniami\
  \ w r\xF3\u017Cnych\u2026"
lastmod: '2024-03-11T00:14:08.686201-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w programowaniu polega na tworzeniu i uruchamianiu skrypt\xF3\
  w, kt\xF3re weryfikuj\u0105, czy kod zachowuje si\u0119 zgodnie z oczekiwaniami\
  \ w r\xF3\u017Cnych\u2026"
title: "Pisanie test\xF3w"
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
