---
title:                "TypeScript: Tests schreiben"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil einer soliden Entwicklungsstrategie. Sie helfen dabei, Fehler frühzeitig zu erkennen, die Codequalität zu verbessern und das Vertrauen in die Funktionsweise der Anwendung zu stärken. Durch das Schreiben von Tests können Entwickler*innen auch sicherstellen, dass Änderungen oder Updates an der Anwendung keine unerwünschten Nebenwirkungen haben.

## Wie man Tests in TypeScript schreibt

Um Tests in TypeScript zu schreiben, gibt es verschiedene Frameworks, wie z.B. Jest oder Mocha. In diesem Beispiel verwenden wir das populäre Test-Framework Jest.

Zunächst müssen wir die Jest-Bibliothek in unserem Projekt installieren:

```TypeScript
npm install jest --save-dev
```

Anschließend können wir unsere erste Test-Datei erstellen, z.B. "add.test.ts". In dieser Test-Datei importieren wir die Funktion, die wir testen möchten, und definieren dann unsere Testfälle:

```TypeScript
import { add } from "./math";

test("addition of two numbers", () => {
  expect(add(2, 4)).toBe(6);
});

test("addition of negative and positive numbers", () => {
  expect(add(-5, 3)).toBe(-2);
});
```

Wir verwenden die global verfügbare Funktion "test" von Jest, um unsere Tests zu definieren. In den Testfällen nutzen wir die "expect" Funktion, um das Verhalten unserer Funktion "add" zu überprüfen. In diesem Fall erwarten wir, dass die Addition von zwei Zahlen das erwartete Ergebnis zurückgibt.

Um unsere Tests auszuführen, können wir einfach den Befehl "npm test" in der Kommandozeile ausführen. Jest wird dann automatisch alle Dateien mit ".test.ts" Endung ausführen und das Ergebnis ausgeben.

## Tieferer Einblick in das Schreiben von Tests

Es gibt verschiedene Arten von Tests, die man schreiben kann, wie z.B. Unit-Tests, Integrationstests oder End-to-End-Tests. Es ist wichtig, zu verstehen, welcher Testtyp für welche Teile der Anwendung am besten geeignet ist.

Außerdem ist es hilfreich, sich mit Konzepten wie Mocking und Stubbing auseinanderzusetzen, um externe Abhängigkeiten bei Tests zu berücksichtigen. Eine gute Testabdeckung ist auch ein entscheidender Faktor in der Qualität und Stabilität einer Anwendung.

## Siehe auch

- [Jest Dokumentation] (https://jestjs.io/docs/de/getting-started)
- [Unit-Tests vs. Integrationstests] (https://www.geeksforgeeks.org/unit-vs-integration-vs-end-to-end-testing)
- [Mocking vs. Stubbing] (https://www.baeldung.com/mockito-vs-easymock-vs-jmockit)