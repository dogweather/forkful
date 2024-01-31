---
title:                "Scrivere test"
date:                  2024-01-19
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test è il processo di creazione di script che verificano il corretto funzionamento del codice. I programmatori scrivono test per assicurarsi che il loro codice funzioni come previsto e per prevenire regressioni.

## How to:
PHP utilizza PHPUnit come framework standard per il testing. Ecco un esempio di test semplice:

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

Esecuzione del test con output di esempio:
```bash
$ ./vendor/bin/phpunit StackTest
OK (1 test, 3 assertions)
```

## Deep Dive
PHPUnit è stato introdotto da Sebastian Bergmann ed è diventato il framework di test più popolare per PHP. Come alternativa, si può usare Behat per test di comportamento o PHPSpec per spec-based testing. Per scrivere test affidabili, si utilizzano assertion che confrontano i risultati attesi con quelli effettivi.

## See Also
- [PHPUnit Manual](https://phpunit.de/manual/current/en/index.html)
- [Behat, BDD framework for PHP](https://docs.behat.org/en/latest/)
- [PHPSpec, spec-based testing framework](http://www.phpspec.net/en/stable/)
