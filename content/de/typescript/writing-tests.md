---
title:    "TypeScript: Tests schreiben"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Softwareentwicklung ist es unerlässlich, qualitativ hochwertigen Code zu schreiben. Eine Methode, um sicherzustellen, dass der Code richtig funktioniert und mögliche Fehler zu erkennen, sind Tests. Diese können manuell oder automatisiert durchgeführt werden und helfen dabei, Bugs frühzeitig zu finden und die Wartbarkeit des Codes zu verbessern.

## Wie man Tests in TypeScript schreibt

Um Tests in TypeScript zu schreiben, kann das Framework Jest verwendet werden. Dieses bietet eine einfache und intuitive Möglichkeit, Tests zu erstellen. Schauen wir uns zunächst die grundlegende Struktur eines Tests an:

```TypeScript
// Importieren des Testframeworks
import * as jest from 'jest';

// Beschreibung und Name des Tests
describe('Funktion addieren', () => {
  // Funktion, die getestet werden soll
  it('sollte zwei Zahlen korrekt addieren', () => {
    const result = addieren(2, 3); // Aufrufen der addieren-Funktion
    expect(result).toBe(5); // Erwartetes Ergebnis
  });
});
```

In diesem Beispiel verwenden wir die Funktion `addieren`, um zu überprüfen, ob sie zwei Zahlen korrekt addiert. Mit `expect` können wir angeben, welches Ergebnis wir erwarten. Jest vergleicht dann automatisch das tatsächliche Ergebnis mit dem Erwarteten und gibt eine Fehlermeldung aus, wenn diese nicht übereinstimmen.

## Deep Dive

Es gibt einige bewährte Methoden, um effektive Tests in TypeScript zu schreiben:

- Schreibe unabhängige Tests, die nur eine Sache auf einmal überprüfen. Dadurch werden sie einfacher zu lesen und zu warten.
- Nutze `beforeEach` oder `afterEach` Hooks, um wiederholte oder vorbereitende Schritte in deinen Tests auszuführen.
- Nutze das mocking-Feature von Jest, um externe Abhängigkeiten zu simulieren, die deine Tests beeinflussen könnten.

Es gibt viele weitere Tipps und Tricks, um effektive Tests zu schreiben, aber diese sollen dir einen guten Einstieg in das Testen in TypeScript geben.

## Siehe auch

- [Offizielle Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Anleitung zur Verwendung von Jest in TypeScript-Projekten](https://kulshekhar.github.io/ts-jest/user/config/)
- [Tutorial zum Testen von TypeScript mit Jest](https://www.valentinog.com/blog/jest/)