---
title:                "Tests schreiben"
html_title:           "Javascript: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil einer guten JavaScript Codebase. Sie helfen nicht nur dabei, Fehler zu identifizieren und zu beheben, sondern tragen auch zur Verbesserung der Codequalität und des Entwicklungsprozesses bei. Durch das Schreiben von Tests können Entwickler sicherstellen, dass ihr Code zuverlässig und robust ist, während sie gleichzeitig Zeit und Geld sparen, indem sie mögliche Probleme frühzeitig erkennen.

## Wie man Tests schreibt

Tests sollten Teil des Entwicklungsprozesses sein. Sie werden üblicherweise mit Hilfe von Test-Driven Development (TDD) geschrieben, bei dem zuerst der Testcode geschrieben wird und dann der eigentliche Code. Hier ist ein Beispiel für eine einfache Funktion "add", die zwei Zahlen addiert:

```Javascript
// Test:
test('Summe von 2 und 3 sollte 5 ergeben', () => {
  expect(add(2, 3)).toBe(5);
});

// Code:
function add(a, b) {
  return a + b;
}

// Ausgabe:
> Summe von 2 und 3 sollte 5 ergeben
Test 1 erfolgreich bestanden
```

In diesem Beispiel wird mithilfe der "test" Funktion ein Test erstellt, der erwartet, dass die Funktion "add" die korrekte Summe ausgibt. Die "expect" Funktion wird verwendet, um das erwartete Ergebnis anzugeben und die "toBe" Funktion überprüft, ob das Ergebnis der Funktion tatsächlich dem Erwarteten entspricht.

Mit dem Ergebnis "Test 1 erfolgreich bestanden" kann der Entwickler sicher sein, dass die "add" Funktion wie erwartet funktioniert. Auf diese Weise können weitere Funktionen und Szenarien getestet werden, um sicherzustellen, dass sie wie gewünscht funktionieren.

## Deep Dive

Tests sind oft Teil des Continuous Integration und Continuous Delivery Prozesses, bei dem Änderungen an Code automatisch getestet werden, bevor sie in die Produktion gelangen. Dadurch wird sichergestellt, dass keine fehlerhaften Änderungen ausgeliefert werden und die Anwendung stabil bleibt.

Es gibt verschiedene Arten von Tests, die in einer JavaScript-Codebase verwendet werden können, wie z.B. Unit Tests, Integration Tests und End-to-End Tests. Jede Art hat ihre eigenen Vorteile und trägt zur Fehlerfreiheit des Codes bei.

Es ist auch wichtig zu beachten, dass Tests niemals die Notwendigkeit von manueller Überprüfung oder Benutzertests ersetzen können. Sie dienen vielmehr als zusätzliche Sicherheit und ermöglichen es Entwicklern, Änderungen schneller und zuverlässiger durchzuführen.

## Siehe auch

- [Einführung in das Testen mit JavaScript](https://www.javascript.com/resources/testing-with-javascript)
- [Test Driven Development erklärt auf Deutsch](https://agile.de/testing/test-driven-development/)
- [Eine Anleitung zur Verwendung von Jest zum Testen von JavaScript Code](https://blog.logrocket.com/a-quick-guide-to-using-jest-for-testing-in-javascript/)