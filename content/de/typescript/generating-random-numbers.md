---
title:                "Erzeugen von Zufallszahlen"
html_title:           "TypeScript: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum?

Die Generierung von Zufallszahlen ist ein unverzichtbarer Bestandteil vieler Programmieraufgaben. Sie kann zum Beispiel in der Spielentwicklung oder bei der Erstellung von Testdaten verwendet werden, um eine realistische oder zufällige Komponente hinzuzufügen.

## Wie geht's?

Code-Beispiel mit TypeScript und Ausgabe:

```TypeScript
// Zufallszahl zwischen 1 und 10 generieren
const randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Ausgabe: 7
```

Um eine Zufallszahl in TypeScript zu generieren, können wir die globalen Methoden `Math.random()` und `Math.floor()` nutzen. Zuerst wird eine Zahl zwischen 0 und 1 mithilfe von `Math.random()` erzeugt. Diese Zahl wird dann mit `Math.floor()` auf die nächste ganze Zahl abgerundet. Durch die Multiplikation mit dem gewünschten Bereich und einer eventuellen Verschiebung können wir die benötigten Zufallszahlen erzeugen.

## Deep Dive

Bei der Generierung von Zufallszahlen sollte man sich bewusst sein, dass diese Werte nicht wirklich zufällig sind. Die meisten Zufallszahlengeneratoren basieren auf einem Algorithmus und können somit ein bestimmtes Muster aufweisen. Um eine höhere Zufälligkeit zu erreichen, kann man beispielsweise den `Math.random()`-Wert mit der aktuellen Zeit oder anderen Variablen kombinieren.

Außerdem ist es wichtig zu beachten, dass diese Methode pseudozufällig ist und nicht für Anwendungen mit hoher Sicherheit, wie z.B. in der Kryptographie, geeignet ist. In solchen Fällen sollten spezielle Bibliotheken oder Dienste zur Generierung von kryptographisch sicheren Zufallszahlen verwendet werden.

## Siehe auch

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [TypeScript Handbook: Math](https://www.typescriptlang.org/docs/handbook/2/modules.html#math)
- [Stack Overflow: Best way to generate random integer](https://stackoverflow.com/questions/1527803/generating-random-whole-numbers-in-javascript-in-a-specific-range)