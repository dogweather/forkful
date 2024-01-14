---
title:                "TypeScript: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

# Warum

Sind Sie bereit, Ihren TypeScript-Code auf die nächste Stufe zu bringen? Dann ist es Zeit, über Tests nachzudenken! Tests sind ein integraler Bestandteil der Softwareentwicklung und helfen dabei, Bugs frühzeitig zu entdecken und die Gesamtqualität des Codes zu verbessern. In diesem Blog-Beitrag erfahren Sie, warum es wichtig ist, Tests zu schreiben und wie Sie damit beginnen können.

## Wie

Um in TypeScript Tests zu schreiben, nutzen wir das Framework Jest. Dieses Framework ermöglicht es uns, einfach zu implementierende und lesbare Tests zu schreiben. Schauen wir uns ein Beispiel an:

```TypeScript
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Hier importieren wir die Funktion "sum" aus unserem Code und definieren einen Test, der überprüft, ob das Ergebnis der Summe von 1 und 2 gleich 3 ist. Mit der "expect" Methode von Jest prüfen wir, ob das Ergebnis der Funktion tatsächlich dem erwarteten Wert entspricht. Wenn alle Tests erfolgreich sind, erhalten wir die Meldung "Test erfolgreich durchgeführt!".

## Deep Dive

Das Schreiben von Tests kann zunächst etwas zeitaufwendig erscheinen, aber es hat langfristig viele Vorteile. Durch das Schreiben von Tests können wir sicherstellen, dass unser Code fehlerfrei funktioniert und durch die Automatisierung der Tests sparen wir Zeit und Mühe. Zudem ermöglichen uns Tests, Änderungen am Code vorzunehmen, ohne dabei die Funktionalität zu beeinträchtigen.

Ein weiterer Vorteil von Tests ist die Dokumentation des Codes. Durch den Ausschnitt von Beispielen in den Tests können Entwickler schnell verstehen, wie die Funktionen und Module in Ihrem Code funktionieren und diese gezielt nutzen.

## Siehe Auch

Hier finden Sie weitere Ressourcen, um tiefer in das Schreiben von Tests einzusteigen:

- [Offizielle Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Tutorial: Getting Started with Jest in TypeScript](https://medium.com/@hardikpthv/getting-started-with-jest-in-typescript-cc1de29cf8cb)
- [Einsteiger-Guide zu TypeScript-Tests](https://blog.bitsrc.io/a-beginners-guide-to-unit-testing-with-typescript-and-jest-263f4bede623)