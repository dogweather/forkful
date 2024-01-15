---
title:                "Tests schreiben."
html_title:           "TypeScript: Tests schreiben."
simple_title:         "Tests schreiben."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil der Softwareentwicklung. Sie ermöglichen es, die Qualität des Codes zu überprüfen und Fehler frühzeitig zu erkennen. Dadurch wird die Gesamtleistung des Projekts verbessert und die Zeiten für das Debugging reduziert, was letztendlich Zeit und Geld spart.

## Wie man Tests schreibt

Um Tests in TypeScript zu schreiben, benötigen Sie ein Test-Framework wie Jest oder Mocha. Diese ermöglichen es, Tests in speziellen Dateien zu schreiben, die mit ".test.ts" oder ".spec.ts" enden.

Hier ist ein Beispiel für eine einfache Funktion, die wir mit Jest testen können:

```TypeScript
// index.ts

function addNumbers(x: number, y: number) {
  return x + y;
}

export { addNumbers };
```

```TypeScript
// index.test.ts
import { addNumbers } from "./index";

describe("addNumbers function", () => {
  it("should return the sum of two numbers", () => {
    const result = addNumbers(5, 7);
    expect(result).toBe(12);
  });
});
```

Nachdem wir die Funktion importiert haben, können wir in der `describe`-Funktion eine Beschreibung des zu testenden Funktionsbereichs angeben. In der `it`-Funktion geben wir dann die konkreten Erwartungen für den Test an, indem wir `expect` und `toBe` verwenden. Wenn der Test erfolgreich ist, wird die Ausgabe "Passed" angezeigt, ansonsten wird eine detaillierte Fehlermeldung ausgegeben.

Natürlich gibt es viele weitere Möglichkeiten, Tests zu schreiben, je nach Bedarf und Komplexität des Codes.

## Deep Dive

Es ist wichtig zu beachten, dass Tests auch als eine Art Dokumentation dienen können. Sie beschreiben, was der Code tun sollte und dienen als Referenz für das Verhalten des Codes. Sie können auch dazu beitragen, gute Praktiken wie eine gute Codeabdeckung zu fördern und sicherzustellen, dass jede Änderung am Code keine ungewollten Nebenwirkungen hat.

Außerdem können Tests auch automatisch ausgeführt werden, jedes Mal wenn der Code geändert wurde. Dies beschleunigt den Feedback-Zyklus und unterstützt eine schnellere Weiterentwicklung des Projekts.

## Siehe auch

- [Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Mocha Dokumentation](https://mochajs.org/)
- [Testgetriebene Entwicklung in TypeScript](https://timdeschryver.dev/blog/test-driven-development-in-typescript)