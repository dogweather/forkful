---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:06.039009-07:00
description: 'Hoe: We duiken in PHPUnit, een populair PHP-testframework. Installeer
  het eerst met Composer.'
lastmod: '2024-03-13T22:44:50.899004-06:00'
model: gpt-4-0125-preview
summary: We duiken in PHPUnit, een populair PHP-testframework.
title: Tests Schrijven
weight: 36
---

## Hoe:
We duiken in PHPUnit, een populair PHP-testframework. Installeer het eerst met Composer:

```bash
composer require --dev phpunit/phpunit
```

Laten we nu een eenvoudige test schrijven. Stel je voor dat je een klasse `Calculator` hebt met een `add` methode.

```php
// Calculator.php
class Calculator {
    public function add($a, $b) {
        return $a + $b;
    }
}
```

Zo test je het:

```php
// CalculatorTest.php
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase {
    public function testAddition() {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```

Voer de test uit met:

```bash
./vendor/bin/phpunit CalculatorTest
```

De output toont of tests slagen of falen.

## Diepere Duik
Testen was niet altijd een grote zaak in PHP. Oorspronkelijk zetten velen snel code in elkaar en controleerden handmatig of het werkte. Nu is testen koning. PHPUnit begon aan populariteit te winnen in de jaren 2000 en is nu bijna standaard. Alternatieven? Zeker, er zijn PHPSpec en Behat, om mee te beginnen. Onder de motorkap gebruikt PHPUnit beweringen om verwachte en werkelijke resultaten te vergelijken, en testdubbels (mocks, stubs, spionnen) om externe afhankelijkheden na te bootsen.

## Zie Ook
- PHPUnit Handboek: https://phpunit.de/manual/current/nl/index.html
- PHP Op De Juiste Manier (Testen): http://www.phptherightway.com/#testing
- Mockery (mock-framework voor PHPUnit): http://docs.mockery.io/en/latest/
