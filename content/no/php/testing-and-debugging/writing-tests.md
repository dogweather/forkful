---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:19.975870-07:00
description: "Hvordan: Et mye brukt verkt\xF8y for testing i PHP er PHPUnit. Installer\
  \ det via Composer."
lastmod: '2024-03-13T22:44:40.890241-06:00'
model: gpt-4-0125-preview
summary: "Et mye brukt verkt\xF8y for testing i PHP er PHPUnit."
title: Skrive tester
weight: 36
---

## Hvordan:


### Nativ PHP – PHPUnit
Et mye brukt verktøy for testing i PHP er PHPUnit. Installer det via Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Skrive en enkel test:
Opprett en `CalculatorTest.php`-fil i en `tests`-mappe:
```php
use PHPUnit\Framework\TestCase;

// Anta at du har en Calculator-klasse som legger sammen tall
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Kjør testene med:
```bash
./vendor/bin/phpunit tests
```

#### Eksempel på output:
```
PHPUnit 9.5.10 av Sebastian Bergmann og bidragsytere.

.                                                                   1 / 1 (100%)

Tid: 00:00.005, Minne: 6.00 MB

OK (1 test, 1 påstand)
```

### Tredjepartsbiblioteker – Mockery
For kompleks testing, inkludert å lage mock-objekter, er Mockery et populært valg.

```bash
composer require --dev mockery/mockery
```

#### Integrere Mockery med PHPUnit:
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
For å kjøre, bruk samme PHPUnit-kommando som ovenfor. Mockery tillater ekspressive og fleksible mock-objekter, noe som letter testing av komplekse interaksjoner innen applikasjonen din.
