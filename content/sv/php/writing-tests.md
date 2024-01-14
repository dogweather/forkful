---
title:                "PHP: Skriva tester"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering. Genom att skriva tester kan du försäkra dig om att din kod fungerar som den ska och hjälper dig att undvika ödesdigra buggar. Det sparar också tid i det långa loppet eftersom du slipper felsöka och fixa problem längre fram.

## Så här gör du

För att skriva tester i PHP kan du använda olika testramverk som till exempel PHPUnit eller Codeception. Här är ett exempel på hur du kan skriva en enkel test med PHPUnit:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calc = new Calculator();
        $result = $calc->add(2, 3);
        $this->assertEquals(5, $result);
    }
}
```

I detta exempel har vi en testklass som heter `CalculatorTest` som ärver från PHPUnit's `TestCase`-klass. Testmetoden `testAdd` skapar en instans av Calculator-klassen och anropar sedan dess `add`-metod med värdena 2 och 3. Till sist använder vi PHP's `assertEquals`-funktion för att kontrollera att resultatet av additionen är 5.

## Djupdykning

Att skriva tester handlar inte bara om att kontrollera att din kod fungerar som den ska. Det kan också hjälpa dig att utforma bättre kod från början genom att tvinga dig att tänka på hur din kod ska användas och vilka resultat du förväntar dig.

Ett annat viktigt koncept inom testning är "mocking". Med hjälp av mocking kan du simulera olika scenarier och kontrollera hur din kod beter sig. Detta kan vara särskilt användbart vid integrationstester där du testar hur olika delar av din kod samverkar.

## Se även

- [PHPUnit dokumentation](https://phpunit.de/documentation.html)
- [Codeception dokumentation](https://codeception.com/docs/)
- [Mocking with PHPUnit](https://phpunit.de/manual/current/en/test-doubles.html)