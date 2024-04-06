---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:24.102120-07:00
description: "Hur man g\xF6r: Ett brett anv\xE4nd verktyg f\xF6r att testa i PHP \xE4\
  r PHPUnit. Installera det via Composer."
lastmod: '2024-03-13T22:44:37.999744-06:00'
model: gpt-4-0125-preview
summary: "Ett brett anv\xE4nd verktyg f\xF6r att testa i PHP \xE4r PHPUnit."
title: Skriva tester
weight: 36
---

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
