---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:19.975870-07:00
description: "\xC5 skrive tester i programmering inneb\xE6rer \xE5 lage og kj\xF8\
  re skript som verifiserer at koden oppf\xF8rer seg som forventet under ulike forhold.\
  \ Programmerere\u2026"
lastmod: 2024-02-19 22:05:00.153683
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i programmering inneb\xE6rer \xE5 lage og kj\xF8re skript\
  \ som verifiserer at koden oppf\xF8rer seg som forventet under ulike forhold. Programmerere\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i programmering innebærer å lage og kjøre skript som verifiserer at koden oppfører seg som forventet under ulike forhold. Programmerere gjør dette for å sikre kvalitet, forhindre regresjoner og legge til rette for sikker refaktorering, noe som er avgjørende for å opprettholde en sunn, skalerbar og feilfri kodebase.

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
