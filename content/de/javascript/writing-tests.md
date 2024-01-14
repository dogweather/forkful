---
title:                "Javascript: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests gehört zu den grundlegenden Aufgaben eines jeden Programmierers. Tests helfen dabei, Fehler in Code frühzeitig zu erkennen und somit die Qualität und Stabilität von Anwendungen zu verbessern. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, warum es wichtig ist, Tests zu schreiben und wie man sie effektiv nutzen kann.

## Wie man Tests schreibt

Um Tests zu schreiben, gibt es verschiedene Frameworks und Bibliotheken, die man verwenden kann. In diesem Beispiel werden wir das weit verbreitete Testing-Framework Jest verwenden. Um loszulegen, müssen wir zuerst Jest über den Paketmanager unserer Wahl installieren.

```Javascript
npm install jest --save-dev
```

Nach der Installation können wir unsere Tests im `__test__` Verzeichnis unseres Projekts erstellen. Jedem Test muss eine Funktion mit dem Namen `test` zugewiesen werden und wir können Assertions verwenden, um zu überprüfen, ob das erwartete Ergebnis erreicht wurde.

```Javascript
// Addition.test.js
test('Adds 1 + 2 to equal 3', () => {
  expect(1 + 2).toBe(3);
});
```
Um unsere Tests auszuführen, können wir das Kommando `npm test` verwenden. Jest wird dann alle Dateien im `__test__` Verzeichnis ausführen und uns mitteilen, ob die Tests erfolgreich waren oder nicht.

## Eine tiefergehende Untersuchung

Tests sind nicht nur hilfreich, um Fehler zu erkennen, sondern sie sind auch nützlich für die Dokumentation von Code. Indem man Tests schreibt, erfasst man automatisch die erwarteten Ergebnisse und kann somit auch in Zukunft schnell überprüfen, ob der Code noch immer die gewünschten Ergebnisse liefert.

Zusätzlich bieten Tests auch eine Form der Sicherheit bei der Entwicklung von neuen Features oder Refactoring von Code. Wenn die Tests fehlschlagen, ist dies ein Hinweis darauf, dass etwas im Code geändert wurde, das dazu führt, dass das erwartete Ergebnis nicht mehr erreicht wird.

Ein weiterer Vorteil von Tests ist, dass sie helfen können, Bugs zu vermeiden und somit die allgemeine Qualität des Codes zu verbessern. Durch das Schreiben von Tests werden verschiedene Teile des Codes automatisch durchlaufen, was dazu beiträgt, mögliche Fehler aufzudecken.

## Siehe auch

- [Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Einführung in das Testen von JavaScript mit Jest](https://opensource.com/article/20/3/javascript-testing-jest)
- [Warum Tests schreiben?](https://medium.com/@js_tutorthewhy/why-test-your-code-dc86ab78b9da)