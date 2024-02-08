---
title:                "Scrivere test"
aliases:
- it/php/writing-tests.md
date:                  2024-02-03T19:31:17.809549-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Scrivere test nella programmazione implica la creazione e l'esecuzione di script che verificano il comportamento del codice come previsto in varie condizioni. I programmatori lo fanno per garantire la qualità, prevenire regressioni e facilitare il rifattorizzamento sicuro, che è cruciale per mantenere una base di codice sana, scalabile e priva di bug.

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
