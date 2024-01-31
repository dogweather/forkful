---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Testen ist das Überprüfen von Code auf Richtigkeit. Programmierer schreiben Tests, um Fehler früh zu erkennen, die Softwarequalität zu sichern und zukünftige Änderungen zu vereinfachen.

## How to:
Ein einfacher Test in Go sieht so aus:

```Go
package main

import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func Add(a, b int) int {
    return a + b
}

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    assert.Equal(t, 3, result, "Add(1, 2) sollte 3 ergeben")
}
```

Führe den Test mit `go test` in deinem Terminal aus. Erwartete Ausgabe:

```
PASS
ok      mypackage  0.002s
```

## Deep Dive
Das Testen in Go hat Wurzeln in den agilen Entwicklungspraktiken, insbesondere Test-Driven Development (TDD). Neben dem Go-eigenen Testing-Paket gibt es Alternativen wie GoConvey oder Ginkgo für BDD-Ansätze. Go-Tests nutzen Reflexion intern, um Testfälle aufzurufen und zu reporten.

## See Also
- Go Testing Dokumentation: https://golang.org/pkg/testing/
- Testify Assertions: https://github.com/stretchr/testify
- GoConvey: http://goconvey.co/
- Ginkgo BDD Framework: https://onsi.github.io/ginkgo/
