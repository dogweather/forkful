---
title:                "Go: Scrivere test"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere dei test in Go
Scrivere test per il proprio codice è essenziale per garantire che esso funzioni correttamente e manterrà le sue funzionalità anche dopo le modifiche. Inoltre, i test possono aiutare a identificare bug e problemi in modo tempestivo, risparmiando tempo e sforzi nella fase di debugging.

## Come scrivere test in Go
Ecco un esempio di codice che mostra come scrivere e eseguire un test in Go:

```Go
package main

import (
	"testing"
)

func Sum(x, y int) int {
	return x + y
}

func TestSum(t *testing.T) {
	result := Sum(2, 3)
	expected := 5
	if result != expected {
		t.Errorf("Expected %d, but got %d", expected, result)
	}
}
```

L'output dovrebbe essere il seguente:

```
$ go test
PASS
ok      example.com/test    0.001s
```

In questo esempio, stiamo testando una semplice funzione Sum che dovrebbe restituire la somma di due numeri interi. Il test verifica che il risultato di Sum(2,3) sia uguale a 5. In caso contrario, l'errore verrà visualizzato.

## Un approfondimento sui test in Go
Quando si scrivono test in Go, è importante seguire alcune buone pratiche:

- Organizza i tuoi test in file separati dalla logica del tuo codice.
- Assicurati che i tuoi test vengano eseguiti in modo indipendente l'uno dall'altro.
- Utilizza il pacchetto "testing" incluso in Go per scrivere e eseguire i tuoi test.
- Utilizza gli assert per verificare che i risultati ottenuti siano quelli attesi.

Ricorda che i test non dovrebbero essere un'opportunità per scrivere codice non testato. Assicurati sempre di scrivere test per tutte le funzionalità critiche del tuo codice.

## Vedi anche
- [Go tutorial sul writing test](https://golang.org/doc/tutorial/add-a-test)
- [Testing best practices in Go](https://onecompiler.com/blog/golang-testing-best-practices)
- [Go testing documentation](https://golang.org/pkg/testing/)