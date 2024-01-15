---
title:                "Skapa tester"
html_title:           "PHP: Skapa tester"
simple_title:         "Skapa tester"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av programmering eftersom det hjälper till att säkerställa att koden fungerar som förväntat och att eventuella buggar upptäcks tidigt. Det sparar tid och minskar risken för oväntade problem i produktionen.

## Hur man gör
Det första steget för att skriva tester är att välja ett testningsverktyg. I PHP är det vanligaste valet PHPUnit. Sedan bör man ha en förståelse för enhetstester och integrationstester. Här är ett exempel på en enkel enhetstest med PHPUnit:

```PHP
<?php
require 'Calculator.php'; // Filen vi vill testa

class CalculatorTest extends PHPUnit_Framework_TestCase {

  public function testAdd() {
    $calc = new Calculator();
    // Förväntat resultat
    $result = $calc->add(2, 5);
    $this->assertEquals(7, $result); // Assertion
  }

}
```

PHPUnit_Framework_TestCase är en grundläggande enhetstestkärning som kommer med PHPUnit och används för att skapa testfall. I exemplet ovan testas en "add" -funktion i en enkel räknareklass. PHPUnit har många olika assertions som kan användas för att kontrollera olika förväntningar.

## Djupdykning
En viktig del av att skriva tester är att täcka så många olika scenarier som möjligt. Detta inkluderar felaktiga indata och gränsvärden. Det är också viktigt att hålla testerna uppdaterade när koden ändras för att säkerställa att de fortfarande ger rätt resultat.

En annan aspekt att tänka på är att skriva tester för skalbara applikationer. Detta innebär att unittests bör vara isolerade och inte påverkas av externt API-anrop eller databasåtgärder. För att testa integrationsflöden kan man använda sig av mockar eller simulerade enheter istället för att faktiskt anropa andra tjänster. Detta kommer att minska risken för falska positiva eller negativa resultat i testerna.

## Se också
- [PHPUnit dokumentation](https://phpunit.de/documentation.html)
- [Enhetstester på laravel.com](https://laravel.com/docs/5.8/testing)
- [Enhetstesting på codecourse.com](https://www.codecourse.com/lessons/phpunit-tutorial-1)