---
title:                "Skriva tester"
aliases:
- /sv/php/writing-tests.md
date:                  2024-02-03T19:31:24.102120-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester i programmering involverar att skapa och köra skript som verifierar att koden beter sig som förväntat under olika förhållanden. Programmerare gör detta för att säkerställa kvalitet, förebygga regressioner och underlätta säker refaktorisering, vilket är avgörande för att underhålla en hälsosam, skalbar och buggfri kodbas.

## Hur man gör:
### Naturlig PHP – PHPUnit
Ett brett använd verktyg för att testa i PHP är PHPUnit. Installera det via Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Skriva ett enkelt test:
Skapa en `CalculatorTest.php`-fil i en `tests`-katalog:
```php
use PHPUnit\Framework\TestCase;

// Antaget att du har en Calculator-klass som adderar siffror
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Kör testerna med:
```bash
./vendor/bin/phpunit tests
```

#### Exempel på utdata:
```
PHPUnit 9.5.10 av Sebastian Bergmann och bidragsgivare.

.                                                                   1 / 1 (100%)

Tid: 00:00.005, Minne: 6,00 MB

OK (1 test, 1 påstående)
```

### Tredjepartsbibliotek – Mockery
För komplexa tester, inklusive att mocka objekt, är Mockery ett populärt val.

```bash
composer require --dev mockery/mockery
```

#### Integrera Mockery med PHPUnit:
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
        $resultat = $service->execute();

        $this->assertEquals('mocked result', $resultat);
    }
}
```
För att köra, använd samma PHPUnit-kommando som ovan. Mockery möjliggör expressiva och flexibla mock-objekt, vilket underlättar testning av komplexa interaktioner inom din applikation.
