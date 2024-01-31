---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus tarkoittaa koodin toiminnan varmistamista automatisoiduin testein. Testit parantavat ohjelmiston laatua ja helpottavat kehitystä.

## How to:
```Go
package main

import (
    "testing"
)

func Sum(a, b int) int {
    return a + b
}

func TestSum(t *testing.T) {
    total := Sum(5, 5)
    if total != 10 {
        t.Errorf("Sum was incorrect, got: %d, want: %d.", total, 10)
    }
}
```
Testin ajo:
```bash
go test
```
Mahdollinen tulostus:
```
PASS
ok  	your/package/name	0.123s
```

## Deep Dive
Go-testaamisen juuret ovat TDD:ssä (Test Driven Development). Vaihtoehtoina Go:ssa on käyttää ulkopuolisia kirjastoja, kuten Testify tai Ginkgo. Yksikkötestit kirjoitetaan samassa paketissa suffiksilla `_test.go` ja käytetään `testing`-kirjastoa.

## See Also
- Go:n dokumentaatio testauksesta: https://golang.org/pkg/testing/
- Testify-kirjaston GitHub-sivu: https://github.com/stretchr/testify
- Tietoa TDD:stä: https://martinfowler.com/bliki/TestDrivenDevelopment.html
