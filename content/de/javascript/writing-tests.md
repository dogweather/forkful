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

## Was & Warum?
Tests schreiben ist ein wichtiger Teil der Programmierung. Durch das Schreiben von Tests können Programmierer sicherstellen, dass ihr Code korrekt funktioniert und mögliche Fehler frühzeitig erkennen. Dadurch wird die Qualität des Codes verbessert und die Entwicklung von Software wird effizienter.

## Wie geht's?
Das Schreiben von Tests in Javascript ist einfach und unkompliziert. Mit der Jest Bibliothek, die Teil des Javascript-Frameworks React ist, können Tests in wenigen Schritten geschrieben werden. Hier ist ein Beispiel, wie man eine Funktion testen kann, die zwei Zahlen addiert:

```Javascript
function add(a, b) {
  return a + b;
}

test('adds two numbers correctly', () => {
  expect(add(2, 3)).toBe(5);
});
```

Zunächst muss die Jest Bibliothek installiert werden: 
```
npm install jest --save-dev
```

Dann kann die Testfunktion geschrieben werden, die den Namen der Funktion, die getestet werden soll, und eine Funktion mit einer Erwartung als Parameter erhält. In diesem Beispiel wird erwartet, dass die Funktion ```add``` die Zahlen 2 und 3 richtig addiert und das Ergebnis 5 zurückgibt.

## Tiefere Einblicke
Tests werden schon seit langem von Programmierern verwendet, um die Qualität ihres Codes zu verbessern und Fehler frühzeitig zu entdecken. Vor der Einführung von automatischen Testframeworks mussten Tests jedoch manuell durchgeführt werden, was viel Zeit und Aufwand erforderte. Mit der Entwicklung von Javascript und der Einführung von Testframeworks wie Jest können Tests jetzt einfach und schnell automatisiert durchgeführt werden.

Es gibt auch alternative Testframeworks für Javascript, wie zum Beispiel Mocha oder Jasmine. Diese bieten ähnliche Funktionen wie Jest, aber mit unterschiedlichen Syntaxen und Herangehensweisen. Es lohnt sich, verschiedene Frameworks auszuprobieren und das zu finden, was am besten zu einem und seinem Team passt.

Um Tests in einem von Jest unterstützten Framework wie React zu schreiben, müssen die Tests in einer Datei mit der Endung ```.test.js``` gespeichert werden. Jest sucht automatisch nach diesen Dateien und führt die Tests aus.

## Siehe auch
- [Jest documentation](https://jestjs.io)
- [Mocha documentation](https://mochajs.org)
- [Jasmine documentation](https://jasmine.github.io)

Happy testing!