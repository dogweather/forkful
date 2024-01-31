---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i programmering betyr å lage kode som kjører applikasjonen din for å sjekke at alt fungerer som forventet. Vi tester for å forutsi og fikse feil, forbedre kvaliteten, og sikre at nye endringer ikke ødelegger eksisterende funksjonalitet.

## Hvordan:
```Go
package main

import (
	"testing"
)

// Enkle funksjon som legger sammen to tall
func Add(a, b int) int {
	return a + b
}

// Testfunksjon for Add
func TestAdd(t *testing.T) {
	result := Add(1, 2)
	if result != 3 {
		t.Errorf("Add(1, 2) feilet, forventet %d, fikk %d", 3, result)
	}
}
```
Kjør testen med `go test` i terminalen. Forventet output:
```
PASS
ok  	path/to/your/package	0.001s
```

## Dypdykk
Testing i Go har blitt en integrert del av språket siden det første ble lansert. Go's innebygde testingpakke `testing` forenkler skriving av unit- og benchmark-tester. Alternativer som GoConvey gir BDD-stil testing, mens Ginkgo tilbyr et mer uttrykksfullt rammeverk. Det anbefales å følge "table-driven tests" mønsteret for å dekke flere tilfeller samtidig.

## Se Også
- Go's offisielle blogg om testing: https://blog.golang.org/testing
- Go test dokumentasjon: https://pkg.go.dev/testing
- GoConvey på GitHub: https://github.com/smartystreets/goconvey
- Ginkgo rammeverket: https://github.com/onsi/ginkgo
