---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:22.157866-07:00
description: 'Kuinka: #.'
lastmod: '2024-03-13T22:44:56.659384-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Testien kirjoittaminen
weight: 36
---

## Kuinka:


### Natiivi PHP – PHPUnit
Laajalti käytetty työkalu PHP:n testaamiseen on PHPUnit. Asenna se Composerin kautta:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Yksinkertaisen testin kirjoittaminen:
Luo `CalculatorTest.php` -tiedosto `tests` -hakemistoon:
```php
use PHPUnit\Framework\TestCase;

// Olettaen, että sinulla on Calculator-luokka, joka lisää numeroita
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Suorita testit komennolla:
```bash
./vendor/bin/phpunit tests
```

#### Näyte tulosteesta:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Aika: 00:00.005, Muisti: 6.00 MB

OK (1 testi, 1 väite)
```

### Kolmannen osapuolen kirjastot – Mockery
Monimutkaiseen testaukseen, mukaan lukien objektien pilkkaaminen, Mockery on suosittu valinta.

```bash
composer require --dev mockery/mockery
```

#### Mockeryn integrointi PHPUnitiin:
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
Suorittaaksesi käytä samaa PHPUnit-komentoa kuin yllä. Mockery mahdollistaa ilmaisuvoimaiset ja joustavat pilkkausobjektit, helpottaen monimutkaisten vuorovaikutusten testaamista sovelluksessasi.
