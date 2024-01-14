---
title:                "Go: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie regelmäßig Go-Code schreiben, werden Sie sicherlich schon von Test-Driven Development (TDD) gehört haben. Aber warum sollten Sie sich die Zeit nehmen, Tests zu schreiben, anstatt einfach direkt mit dem Codieren zu beginnen? Die Antwort ist einfach: Tests helfen Ihnen, die Qualität Ihres Codes zu verbessern, Bugs frühzeitig zu erkennen und Ihre Entwicklungsprozesse zu verbessern.

## Wie

Um Tests in Go zu schreiben, verwenden wir das Paket "testing". Hier ist ein einfaches Beispiel, um zu zeigen, wie es funktioniert:

```Go
func Add(a, b int) int {
    return a + b
}

func TestAdd(t *testing.T) {
    result := Add(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Expected %d, got %d", expected, result)
    }
}
```

Dieser Code definiert eine Funktion "Add", die zwei Zahlen addiert, und einen Test, der überprüft, ob das Ergebnis der Funktion dem erwarteten Wert entspricht. Wir können den Test ausführen, indem wir "go test" in der Terminal eingeben.

Das Output sollte wie folgt aussehen:

```
--- FAIL: TestAdd (0.00s)
    main_test.go:9: Expected 5, got 6
FAIL
```

Hier können wir sehen, dass unser Test fehlgeschlagen ist, was bedeutet, dass wir einen Fehler in unserer Funktion haben und sie korrigieren müssen.

## Deep Dive

Wenn es um das Schreiben von Tests geht, gibt es viele Techniken und Methoden, die Sie anwenden können. Eine wichtige Sache zu beachten ist, dass Ihre Tests so einfach und unabhängig wie möglich sein sollten. Das bedeutet, dass Sie sie nicht von anderen Tests abhängig machen und sie auch nicht zu komplex machen sollten.

Ein weiterer wichtiger Punkt ist, dass die Anzahl Ihrer Tests nicht immer wichtiger ist als ihre Qualität. Fokussieren Sie sich auf die kritischen Teile des Codes und schreiben Sie Tests dafür, anstatt jede einzelne Funktion zu testen.

Es gibt auch verschiedene Tools, die Ihnen helfen können, bessere Tests zu schreiben, wie zum Beispiel "go test -cover", das Ihnen zeigt, wie viel Prozent Ihres Codes von Ihren Tests abgedeckt wird. Je höher die Abdeckung, desto besser.

## Siehe auch

- [Official Go testing package documentation](https://golang.org/pkg/testing/)
- [Effective Go (Testing section)](https://golang.org/doc/effective_go.html#testing)
- [The Go Testing Toolbox](https://github.com/golang/go/wiki/Testing)
  - [GoConvey](https://github.com/smartystreets/goconvey)
  - [Testify](https://github.com/stretchr/testify)
  - [Ginkgo](https://github.com/onsi/ginkgo)
  - [Mockery](https://github.com/vektra/mockery)