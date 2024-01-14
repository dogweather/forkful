---
title:                "PHP: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig og verdifull del av PHP-programmering. Det hjelper deg med å sikre kvalitet og pålitelighet i koden din, og kan bidra til å identifisere og løse feil på en rask og effektiv måte.

## Hvordan

Å skrive tester i PHP kan virke skremmende for mange, men det trenger ikke å være det. La oss se på et enkelt eksempel på hvordan du kan skrive og kjøre en test ved hjelp av PHPUnit:

```PHP
<?php
require 'Calculator.php';

class CalculatorTest extends PHPUnit_Framework_TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(3, $calculator->add(1, 2));
    }
}
?>
```

I dette eksemplet oppretter vi en testklasse og en testmetode som kontrollerer om funksjonen for å legge sammen tall i `Calculator`-klassen fungerer riktig. Output av denne testen vil være `OK (1 test, 1 assertion)`, noe som indikerer at testen er bestått.

## Dykk dypere

Å skrive tester handler ikke bare om å kjøre enkle tester og sjekke om de passerer eller ikke. Det er et viktig konsept å forstå hvordan man skriver gode og effektive tester. Dette inkluderer å lære å skrive modulforkastelser, mock objekter og funksjonelle tester. Jo bedre du forstår disse konseptene, jo bedre blir testene dine.

## Se også

- [PHPUnit dokumentasjon](https://phpunit.de/documentation.html)
- [Tutorial: Basic Unit Testing](https://www.tutorialspoint.com/phpunit/phpunit_basic_unit_testing.htm)
- [7 Tips for Writing Better Unit Tests](https://blog.gurock.com/unit-testing-tips-best-practices/)