---
title:                "Tests Schreiben"
aliases:
- /de/php/writing-tests.md
date:                  2024-02-03T19:31:14.581182-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Tests in der Programmierung zu schreiben, bedeutet, Skripte zu erstellen und auszuführen, die überprüfen, ob der Code sich unter verschiedenen Bedingungen wie erwartet verhält. Programmierer tun dies, um Qualität zu gewährleisten, Regressionen zu verhindern und sicheres Refactoring zu erleichtern, was für die Aufrechterhaltung einer gesunden, skalierbaren und fehlerfreien Codebasis entscheidend ist.

## Wie:
### Native PHP – PHPUnit
Ein weit verbreitetes Werkzeug für Tests in PHP ist PHPUnit. Installieren Sie es über Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Einen einfachen Test schreiben:
Erstellen Sie eine Datei `CalculatorTest.php` im Verzeichnis `tests`:
```php
use PHPUnit\Framework\TestCase;

// Angenommen, Sie haben eine Calculator-Klasse, die Zahlen addiert
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Führen Sie die Tests mit folgendem Befehl aus:
```bash
./vendor/bin/phpunit tests
```

#### Beispiel-Ausgabe:
```
PHPUnit 9.5.10 von Sebastian Bergmann und Mitwirkenden.

.                                                                   1 / 1 (100%)

Zeit: 00:00.005, Speicher: 6.00 MB

OK (1 Test, 1 Behauptung)
```

### Drittanbieter-Bibliotheken – Mockery
Für komplexe Tests, einschließlich des Mockings von Objekten, ist Mockery eine beliebte Wahl.

```bash
composer require --dev mockery/mockery
```

#### Integration von Mockery mit PHPUnit:
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
        $externalServiceMock->shouldReceive('process')->once()->andReturn('gemocktes Ergebnis');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('gemocktes Ergebnis', $result);
    }
}
```
Zum Ausführen verwenden Sie denselben PHPUnit-Befehl wie oben. Mockery ermöglicht ausdrucksstarke und flexible Mock-Objekte, was das Testen von komplexen Interaktionen innerhalb Ihrer Anwendung erleichtert.
