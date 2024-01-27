---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, Code zu erstellen, der überprüft, ob andere Teile des Codes wie erwartet funktionieren. Programmierer machen das, um Fehler früh zu entdecken, die Softwarequalität zu sichern und spätere Änderungen sicher und einfach zu machen.

## How to:
Ein Beispiel für einen einfachen PHPUnit-Test:

```php
<?php
use PHPUnit\Framework\TestCase;

class StackTest extends TestCase
{
    public function testPushAndPop()
    {
        $stack = [];
        $this->assertSame(0, count($stack));

        array_push($stack, 'foo');
        $this->assertSame('foo', $stack[count($stack)-1]);
        $this->assertSame(1, count($stack));

        $this->assertSame('foo', array_pop($stack));
        $this->assertSame(0, count($stack));
    }
}
```
Lauf den Test mit `./vendor/bin/phpunit tests`.

Erwartete Ausgabe:

```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:01.234, Memory: 24.00 MB

OK (1 test, 3 assertions)
```

## Deep Dive:
Tests gab es so lange wie Softwareentwicklung, aber moderne Praktiken wie TDD (Test Driven Development) formten sich in den 2000ern. Alternativen zu PHPUnit sind PHPSpec, Behat und Codeception. Wichtig bei Tests sind Isolation (ein Test darf nicht von anderen abhängen), Wiederholbarkeit (ein Test soll immer das gleiche Ergebnis produzieren) und Automatisierung (Tests sollen sich automatisch ausführen lassen).

## See Also:
- [PHPUnit – The PHP Testing Framework](https://phpunit.de/)
- [Test-Driven Development](https://www.agilealliance.org/glossary/tdd/)
