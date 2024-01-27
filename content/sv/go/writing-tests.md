---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester innebär att koda små program som kontrollerar att din andra kod gör vad den ska. Programmerare använder tester för att undvika fel, förenkla uppdateringar och säkra kodkvaliteten.

## Hur gör man:
```go
package main

import (
    "testing"
)

// Simpel funktion att testa
func Add(a, b int) int {
    return a + b
}

// Testfunktion
func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Kör testet med `go test` i terminalen. Bra utfall:
```
PASS
ok  	example.com/yourpackage	0.001s
```

## Fördjupning
Tester i Go har sina rötter i TDD (Test-Driven Development), en metodik där man skriver testerna först. Alternativ till Go:s inbyggda testpaket inkluderar ramverk som "Testify" eller "Ginkgo". Testerna kan köra i en verklig miljö eller med hjälp av mocking för att simulera beroenden.

## Se även:
- Go testing documentation: https://pkg.go.dev/testing
- En artikel om TDD: https://medium.com/@peterjmag/test-driven-development-what-it-is-and-what-it-is-not-41a6f4f8e3f0
- Go mock-ramverk: https://github.com/golang/mock
