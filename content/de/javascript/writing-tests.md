---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, Code-Stücke zu erstellen, die überprüfen, ob andere Code-Stücke wie erwartet funktionieren. Programmierer machen das, um Fehler zu vermeiden, die Zuverlässigkeit der Software zu steigern und die Wartung zu erleichtern.

## How to:
```Javascript
// Einfacher Funktionstest mit Jest
const addieren = (a, b) => a + b;

test('addieren liefert korrekte Summe', () => {
  expect(addieren(1, 2)).toBe(3);
});
```
Ausgabe:
```bash
PASS  ./addieren.test.js
✓ addieren liefert korrekte Summe (5ms)
```

## Deep Dive
Testen in JavaScript hat sich mit Frameworks wie Jasmine, Mocha und Jest entwickelt. Alternativen zu Jest sind beispielsweise Mocha mit Chai oder Jasmine, die unterschiedliche Syntax und Zusatzfunktionen bieten. Wichtig beim Testen ist die Unterscheidung zwischen Unit-Tests, welche einzelne Funktionen prüfen, und Integrationstests, die das Zusammenspiel mehrerer Komponenten testen.

## See Also
- [Jest](https://jestjs.io/) - Ein umfangreiches Test-Framework für JavaScript.
- [Mocha](https://mochajs.org/) - Ein flexibles Test-Framework.
- [Chai](https://www.chaijs.com/) - Eine Assertion-Bibliothek, die oft mit Mocha verwendet wird.
