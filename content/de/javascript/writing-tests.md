---
title:                "Javascript: Tests schreiben"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests zu schreiben ist ein wichtiger Aspekt beim Entwickeln von Javascript Anwendungen. Sie helfen dabei, Bugs frühzeitig zu erkennen, die Qualität des Codes zu verbessern und die Stabilität der Anwendung zu gewährleisten. Außerdem können Tests dabei helfen, Vertrauen in den Code aufzubauen und die Wartbarkeit des Systems zu erhöhen.

## Wie das geht

Um Tests in Javascript zu schreiben, gibt es verschiedene Frameworks wie zum Beispiel Jest, Mocha oder Jasmine. Für dieses Beispiel werden wir Jest verwenden. Zunächst müssen wir Jest installieren, dies geht entweder über npm oder yarn:

```
npm install jest
```

oder

```
yarn add jest
```

Anschließend können wir unsere erste Testdatei erstellen, zum Beispiel `calculator.test.js`:

```
const calculator = require('./calculator');

test('adds 1 + 2 to equal 3', () => {
  expect(calculator.add(1, 2)).toBe(3);
});
```

In diesem Beispiel importieren wir unsere `calculator` Funktion und führen dann einen Test aus, der erwartet, dass die Addition von 1 und 2 den Wert 3 ergibt. Wenn wir den Test jetzt laufen lassen, wird er erfolgreich sein. Wenn wir jedoch die `add`-Funktion ändern, sodass sie jetzt 1 und 2 zu 4 addiert, wird der Test fehlschlagen und uns darauf aufmerksam machen, dass etwas nicht stimmt.

## Tiefer in die Materie

Es gibt verschiedene Arten von Tests, die wir schreiben können, um sicherzustellen, dass unser Code wie erwartet funktioniert. Zum Beispiel gibt es Unit-Tests, die einzelne Funktionen oder Module testen, und Integrationstests, die überprüfen, ob verschiedene Teile der Anwendung korrekt zusammenarbeiten.

Es gibt auch verschiedene Testing Tools und Techniken, wie zum Beispiel Mocking, um externe Abhängigkeiten zu simulieren, oder Test Driven Development (TDD), bei dem Tests vor dem eigentlichen Code geschrieben werden.

Es ist wichtig, beim Schreiben von Tests auch auf gute Testabdeckung zu achten, sodass möglichst alle Szenarien abgedeckt werden und Bugs frühzeitig erkannt werden können.

## Siehe auch

- [Jest Dokumentation](https://jestjs.io/docs/getting-started)
- [Mocha Webseite](https://mochajs.org/)
- [Jasmine Dokumentation](https://jasmine.github.io/)