---
title:                "PHP: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester för din PHP-kod kan verka som en tråkig och onödig uppgift, men det kan faktiskt vara en livräddare i det långa loppet. Tester hjälper till att upptäcka buggar och problem tidigt i utvecklingsfasen, vilket sparar tid och pengar i framtiden. Dessutom gör det det lättare för andra utvecklare att förstå din kod och göra ändringar i framtiden.

## Hur man gör

För att skriva tester för din PHP-kod behöver du en testningsramverk som PHPUnit, som är den mest populära ramverket för PHP. Efter att ha installerat PHPUnit, skapar du en fil för dina tester och börjar skriva dina tester. Här är ett enkelt exempel:

```PHP
<?php
use PHPUnit\Framework\TestCase;
require 'classToTest.php';

class ClassToTestTest extends TestCase {
  public function testAddNumbers() {
    $class = new ClassToTest();
    $this->assertEquals(5, $class->addNumbers(2, 3));
  }
}
```

I exemplet ovan skapar vi ett test för en klass som heter "ClassToTest". Vi testar funktionen "addNumbers" och förväntar oss att den ska returnera 5 om vi matar in 2 och 3 som parametrar. Om testet misslyckas kommer PHPUnit att visa ett meddelande om vad som gick fel och i vilken fil och rad koden finns på.

## Djupt dyk

Att skriva effektiva tester handlar inte bara om att skriva kod och köra dem. Det handlar också om att följa bästa praxis och skapa lättlästa och underhållbara tester. Här är några tips för att skriva bättre tester:

- Ha en logisk struktur för dina tester: Organisera dina tester så att det är lätt att hitta och förstå dem.
- Testa gränserna: Testa inte bara för typiska värden, utan testa också för extrema eller ovanliga värden.
- Använd mock-objekt: Om din kod är beroende av andra klasser eller funktioner, använd mock-objekt för att isolera koden och fokusera på en specifik del av den.

## Se även

- [PHPUnit dokumentation](https://phpunit.de/documentation.html)
- [PHPUnit tutorial på svenska](https://www.phpunit.de/getting-started/phpunit-7.html)