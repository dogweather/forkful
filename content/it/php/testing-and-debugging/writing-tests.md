---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:17.809549-07:00
description: "Come fare: Uno strumento ampiamente utilizzato per i test in PHP \xE8\
  \ PHPUnit. Installalo tramite Composer."
lastmod: '2024-03-13T22:44:43.520164-06:00'
model: gpt-4-0125-preview
summary: "Uno strumento ampiamente utilizzato per i test in PHP \xE8 PHPUnit."
title: Scrivere test
weight: 36
---

## Come fare:


### PHP Nativo - PHPUnit
Uno strumento ampiamente utilizzato per i test in PHP è PHPUnit. Installalo tramite Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Scrivere un test semplice:
Crea un file `CalculatorTest.php` in una directory `tests`:
```php
use PHPUnit\Framework\TestCase;

// Assumendo che tu abbia una classe Calculator che somma numeri
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Esegui i test con:
```bash
./vendor/bin/phpunit tests
```

#### Esempio di output:
```
PHPUnit 9.5.10 di Sebastian Bergmann e contributori.

.                                                                   1 / 1 (100%)

Tempo: 00:00.005, Memoria: 6.00 MB

OK (1 test, 1 asserzione)
```

### Librerie di Terze Parti – Mockery
Per test complessi, inclusi gli oggetti mock, Mockery è una scelta popolare.

```bash
composer require --dev mockery/mockery
```

#### Integrare Mockery con PHPUnit:
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
        $externalServiceMock->shouldReceive('process')->una volta()->eRitorna('risultato simulato');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('risultato simulato', $result);
    }
}
```
Per eseguire, utilizza lo stesso comando PHPUnit di sopra. Mockery consente oggetti mock espressivi e flessibili, facilitando il test delle interazioni complesse all'interno della tua applicazione.
