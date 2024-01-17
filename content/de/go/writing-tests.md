---
title:                "Tests schreiben"
html_title:           "Go: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests zu schreiben bedeutet, dass Programmierer ihre Code-Basis auf Fehler und Bugs überprüfen. Das hilft dabei, die Qualität des Codes zu verbessern und sicherzustellen, dass er wie erwartet funktioniert.

## Wie geht's?

Um Tests in Go zu schreiben, verwende das "testing" Paket. Mit dem Befehl "go test" können dann alle Tests ausgeführt werden. Hier ist ein Beispiel:

```Go
func Sum(a int, b int) int {
    return a + b
}

func TestSum(t *testing.T) {
    result := Sum(2, 3)
    if result != 5 {
        t.Errorf("Sum(2, 3) should equal 5, but got %d", result)
    }
}
```

Die Ausgabe wäre:

```
--- FAIL: TestSum (0.00s)
    main_test.go:8: Sum(2, 3) should equal 5, but got 6
FAIL
exit status 1
FAIL    _/Users/user1/test  0.006s
```

## Tiefergehende Infos

Tests in Code zu integrieren ist eine gängige Praxis in der Software-Entwicklung und wird oft als Teil des "Test-driven Developments" verwendet. Alternativen zu Go's "testing" Paket beinhalten "gotest.tools" und "testify". Um mehr über Implementierungsdetails zu erfahren, kannst du die offizielle Dokumentation zu Go's "testing" Paket lesen.

## Siehe auch

Weiterführende Infos und Beispiele findest du hier:
- [https://golang.org/pkg/testing/](https://golang.org/pkg/testing/)
- [https://pkg.go.dev/gotest.tools](https://pkg.go.dev/gotest.tools)
- [https://pkg.go.dev/github.com/stretchr/testify](https://pkg.go.dev/github.com/stretchr/testify)