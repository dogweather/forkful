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

# Warum

Tests schreiben kann den Prozess des Codierens erschweren und manchmal sogar etwas nervig sein, aber sie sind unerlässlich, um sicherzustellen, dass dein Code funktioniert und keine unerwünschten Nebeneffekte hat. Tests helfen auch dabei, Fehler frühzeitig zu erkennen und zu verhindern, dass sie in die Produktion gelangen.

# Wie geht man vor

Um einen einfachen Test in Go zu schreiben, musst du zunächst das `testing` Paket importieren. Dann kannst du die Funktion `testing.T` nutzen, um einen Testfall zu definieren.

```Go
import "testing"

func TestAddition(t *testing.T) {
    result := 2 + 2
    expected := 4
    if result != expected {
        t.Errorf("Erwartetes Ergebnis war %d, erhaltenes Ergebnis war %d", expected, result)
    }
}
```

Du kannst die Methode `t.Errorf()` nutzen, um eine Fehlermeldung zu erhalten, wenn das Ergebnis nicht den Erwartungen entspricht.

# Tiefen-Eintauchen

Beim Schreiben von Tests in Go gibt es ein paar wichtige Konzepte zu beachten. Eine davon ist die Verwendung von **table-driven tests**. Das bedeutet, dass du verschiedene Eingaben und erwartete Ergebnisse in einer Tabelle definieren kannst und dann durch alle Kombinationen testen kannst. Dies ist besonders nützlich, um sicherzustellen, dass dein Code für verschiedene Szenarien funktioniert.

Du kannst auch **Mocking** nutzen, um externe Abhängigkeiten zu ersetzen und somit unabhängige Tests zu schreiben. Das `testing` Paket bietet auch Methoden wie `t.Fatal()` und `t.Skip()` an, um deine Tests zu verbessern.

# Siehe auch

- Offizielle Dokumentation für das `testing` Paket in Go: https://golang.org/pkg/testing/
- Ein Tutorial zu TDD (Test-driven Development) in Go: https://blog.alexellis.io/golang-writing-unit-tests/
- Ein Artikel über table-driven tests in Go: https://dave.cheney.net/2013/06/09/writing-table-driven-tests-in-go