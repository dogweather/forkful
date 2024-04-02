---
date: 2024-01-26 01:40:34.287024-07:00
description: "Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes,\
  \ ohne sein externes Verhalten zu \xE4ndern. Programmierer machen das, um die\u2026"
lastmod: '2024-03-13T22:44:54.274466-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes,\
  \ ohne sein externes Verhalten zu \xE4ndern. Programmierer machen das, um die\u2026"
title: Refactoring
weight: 19
---

## Was & Warum?
Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes, ohne sein externes Verhalten zu ändern. Programmierer machen das, um die nichtfunktionalen Attribute der Software zu verbessern. Dadurch wird der Code sauberer und effizienter, was wiederum die Wartung vereinfacht und zukünftige Feature-Ergänzungen erleichtert.

## Wie geht das:

Schauen wir uns ein einfaches Beispiel an, bei dem Refactoring Ihren Code konziser und lesbarer machen kann. Hier refaktorisieren wir eine Funktion, die die Summe eines Arrays von Zahlen berechnet.

Vorher:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Ausgabe: 10
```

Nachher:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Ausgabe: 10
```

Sehen Sie, wie die `reduce` Methode die Größe der Funktion reduziert, während die Funktionalität intakt bleibt? Das ist Refactoring für Sie.

## Tiefer Eintauchen

Refactoring wurde als formale Praxis erst mit der Veröffentlichung von Martin Fowlers Buch "Refactoring: Verbesserung des Designs bestehender Code" im Jahr 1999 etabliert. Dieses Buch, zusammen mit dem Aufkommen agiler Softwareentwicklung, half dabei, Refactoring in den Mainstream zu bringen.

Refactoring als einen Aspekt der Softwareentwicklung zu beschreiben, ist wie zu erklären, warum man eine Werkstatt aufräumen würde: man macht es, damit man das nächste Mal, wenn man etwas reparieren muss (in diesem Fall Code), weniger Zeit mit dem Durcheinander verbringt und mehr mit dem eigentlichen Problem.

Wenn wir über Alternativen zum Refactoring sprechen, betreten wir eine breitere Diskussion über Strategien zur Softwarewartung. Man könnte sich zum Beispiel für eine komplette Neuschreibung entscheiden, aber das ist oft kostspieliger und riskanter. Führen Sie Refactoring schrittweise durch, und Sie ernten laufende Vorteile, ohne das Schiff durch eine plötzliche Generalüberholung zu versenken.

Die Entwicklung von integrierten Entwicklungsumgebungen (IDEs) und Tools wie JSHint, ESLint und Prettier im JavaScript-Ökosystem, die automatische Qualitätsprüfungen des Codes durchführen und Möglichkeiten für Refactoring hervorheben, hat das Refactoring unterstützt.

Es geht alles um sauberen, ausdrucksstarken und wartbaren Code. Komplexe Algorithmen, Optimierungen der Datenstruktur oder sogar architektonische Änderungen, wie der Wechsel von prozeduralen zu funktionalen Programmierstilen, könnten Teil eines Refactorings sein.

Refactoring muss sorgfältig durchgeführt werden; es ist wesentlich, einen robusten Satz von Tests zu haben, um sicherzustellen, dass Ihre Änderungen das Verhalten der Software nicht unerwartet geändert haben – ein weiterer Grund, warum sich Test-Driven Development (TDD) gut mit Refactoring verbindet, da es standardmäßig dieses Sicherheitsnetz bietet.

## Siehe auch

- Martin Fowlers Refactoring Buch: [Refactoring - Verbesserung des Designs bestehender Code](https://martinfowler.com/books/refactoring.html)
- JavaScript-Test-Frameworks (um sicherzustellen, dass Refactoring die Funktionalität nicht bricht):
  - Jest: [Jest - Entzückendes JavaScript-Testing](https://jestjs.io/)
  - Mocha: [Mocha - das spaßige, einfache, flexible JavaScript-Test-Framework](https://mochajs.org/)

- Tools für Code-Qualität und Refactoring-Unterstützung:
  - ESLint: [ESLint - Konfigurierbarer JavaScript-Linter](https://eslint.org/)
  - Prettier: [Prettier - Meinungsbildender Code-Formatter](https://prettier.io/)
