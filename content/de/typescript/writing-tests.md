---
title:    "TypeScript: Tests schreiben"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Softwareentwicklung haben Tests einen wichtigen Platz eingenommen. Sie werden verwendet, um sicherzustellen, dass der Code richtig funktioniert und um Fehler zu entdecken, bevor sie in Produktion gehen. Das Schreiben von Tests kann zwar etwas zusätzliche Zeit und Mühe erfordern, aber es spart langfristig Zeit und verhindert mögliche Fehler, die ansonsten teuer werden könnten.

## Wie man Tests in TypeScript schreibt

Um Tests in TypeScript zu schreiben, müssen wir zuerst ein Test-Framework wie zum Beispiel Jest oder Mocha installieren. Anschließend können wir unsere Tests schreiben, indem wir die assert-Methode des jeweiligen Frameworks nutzen. Sehen wir uns hier ein einfaches Beispiel an:

```TypeScript
// unsere Funktion, die wir testen wollen
function add(x: number, y: number) {
  return x + y;
}

// ein Testfall
test("Addition von zwei Zahlen", () => {
  assert(add(2, 3) === 5); // erwartetes Ergebnis ist 5
}
```

Das assert-Statement überprüft, ob die Funktion add tatsächlich das erwartete Ergebnis, in diesem Fall 5, liefert. Ansonsten würde der Test fehlschlagen und uns darüber informieren, dass etwas nicht wie erwartet funktioniert.

## Tiefer Einblick

Neben einfachen assert-Statements gibt es noch viele weitere Möglichkeiten, Tests in TypeScript zu schreiben. Zum Beispiel können wir unsere Tests in verschiedene Kategorien oder Testgruppen unterteilen, um eine bessere Übersicht und Struktur zu haben. Außerdem können wir Mocking-Techniken nutzen, um externe Abhängigkeiten zu simulieren und unsere Tests unabhängig zu halten. Es gibt auch die Möglichkeit, asynchrone Tests zu schreiben, indem wir Promises oder async/await verwenden.

Das Schreiben von Tests ist nicht nur wichtig, um sicherzustellen, dass der Code richtig funktioniert, sondern es fördert auch eine bessere Code-Qualität und Sichtbarkeit. Indem wir Tests schreiben, können wir sicherstellen, dass unser Code gut getestet und von anderen Entwicklern besser verständlich ist.

## Siehe auch

- [Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Mocha Dokumentation](https://mochajs.org/)
- [Vergleich von Jest und Mocha](https://medium.com/@raj_sinha/choosing-a-javascript-testing-library-jest-vs-mocha-f87130f3478e)