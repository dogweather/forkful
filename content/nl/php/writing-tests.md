---
title:                "Tests Schrijven"
date:                  2024-01-28T22:13:06.039009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Testen controleert of je code doet wat het moet doen. Het bespaart tijd door vroegtijdig fouten op te vangen en zorgt ervoor dat code-wijzigingen niets breken.

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
