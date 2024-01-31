---
title:                "Scrivere test"
date:                  2024-01-19
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Scrivere test significa creare casi specifici per controllare che il codice funzioni come previsto. I programmatori scrivono test per assicurarsi che il loro codice sia corretto e per prevenire future regressioni.

## How to: (Come fare:)
```Go
package main

import (
    "testing"
    "fmt"
)

func Sum(a int, b int) int {
    return a + b
}

func TestSum(t *testing.T) {
    total := Sum(5, 5)
    if total != 10 {
       t.Errorf("Sum was incorrect, got: %d, wanted: %d.", total, 10)
    }
}

func ExampleSum() {
    fmt.Println(Sum(5, 5))
    // Output: 10
}
```

Eseguendo `go test` otterrai:
```
PASS
ok      path/to/your/package    0.002s
```

Se il test fallisce, vedrai qualcosa simile:
```
--- FAIL: TestSum (0.00s)
    sum_test.go:12: Sum was incorrect, got: 9, wanted: 10.
FAIL
exit status 1
FAIL    path/to/your/package    0.002s
```

## Deep Dive (Approfondimento)
Il testing in Go ha radici nel movimento del software agile e nel TDD (Test-Driven Development). Una delle alternative al testing standard con il pacchetto `testing` è l'uso di framework come `Testify` o `GoConvey` per approcci più espressivi e funzionalità aggiuntive. A livello di implementazione, Go si affida a convenzioni, come i file di test che terminano in `_test.go` e le funzioni di test che iniziano con `Test`, per organizzare e rilevare automaticamente i test.

## See Also (Vedi Anche)
- [Documentazione ufficiale su Testing in Go](https://golang.org/pkg/testing/)
- [Esempi di Testify](https://github.com/stretchr/testify)
- [Introduzione a GoConvey](http://goconvey.co/)
- [Articolo su TDD in Go](https://ieftimov.com/post/testing-in-go-go-test/)
