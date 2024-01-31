---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester betyr å lage skript som sjekker om koden virker som forventet. Programmerere gjør dette for å avdekke feil, sikre kvalitet og lette fremtidige endringer.

## Hvordan:
```PHP
<?php
use PHPUnit\Framework\TestCase;

class StackTest extends TestCase
{
    public function testPushAndPop()
    {
        $stack = [];
        $this->assertEquals(0, count($stack));

        array_push($stack, 'foo');
        $this->assertEquals('foo', $stack[count($stack) - 1]);
        $this->assertEquals(1, count($stack));

        $this->assertEquals('foo', array_pop($stack));
        $this->assertEquals(0, count($stack));
    }
}
?>
```
Kjøre test:
```
$ ./vendor/bin/phpunit StackTest
```
Forventet resultat:
```
OK (1 test, 3 assertions)
```

## Dypdykk:
PHP Unit er standarden for enhetstesting i PHP og ble først utgitt i 1997. Alternativer inkluderer Codeception og PHPSpec, men PHP Unit er fortsatt mest brukt. Det bruker asserjoner for å sjekke at kode oppfører seg som forventet og sørger for at endringer ikke bryter eksisterende funksjonalitet.

## Se Også:
- [PHPUnit Manual](https://phpunit.de/manual/current/en/)
- [PHP: The Right Way - Testing](https://phptherightway.com/#testing)
- [Martin Fowler - TestPyramid](https://martinfowler.com/articles/practical-test-pyramid.html)
