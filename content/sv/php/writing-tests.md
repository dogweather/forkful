---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester innebär att skapa kodsatser som automatiskt kontrollerar andra delar av din kod för fel. Programmerare gör det för att säkerställa att deras kod fungerar som förväntat och för att göra framtida underhåll och uppgraderingar enklare och säkrare.

## How to:
PHPUnitTest är ett populärt verktyg för att skriva tester i PHP. Här är ett enkelt exempel:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class StackTest extends TestCase
{
    public function testPushAndPop()
    {
        $stack = [];
        $this->assertSame(0, count($stack));

        array_push($stack, 'foo');
        $this->assertSame('foo', $stack[count($stack) - 1]);
        $this->assertSame(1, count($stack));

        $this->assertSame('foo', array_pop($stack));
        $this->assertSame(0, count($stack));
    }
}
?>
```

Kör testet med PHPUnit för att se följande resultat:

```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:01.042, Memory: 6.00 MB

OK (1 test, 3 assertions)
```

## Deep Dive:
Testning av kod har funnits sen programmeringens begynnelse. Alternativ till PHPUnit inkluderar Codeception, PHPSpec, och Behat. Implementationen av tester varierar beroende på vilket ramverk eller verktyg du använder, men konceptet förblir detsamma: isolera kodstycken och verifiera deras beteende under förutbestämda förhållanden.

## See Also:
- PHPUnits officiella dokumentation: [https://phpunit.de/manual/current/en/index.html](https://phpunit.de/manual/current/en/index.html)
- Test-Driven Development (TDD) på Wikipedia: [https://sv.wikipedia.org/wiki/Testdriven_utveckling](https://sv.wikipedia.org/wiki/Testdriven_utveckling)
- Behat, ett PHP ramverk för beteendedriven utveckling: [http://behat.org/](http://behat.org/)
- PHPSpec, ett domänspecifikt språk för PHP: [http://www.phpspec.net/](http://www.phpspec.net/)
