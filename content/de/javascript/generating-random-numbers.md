---
title:                "Javascript: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von Zufallszahlen ist ein wichtiger Bestandteil der Entwicklung von Programmen und Spielen. Sie ermöglichen eine Vielzahl von Funktionen, wie die Generierung von zufälligen Ereignissen oder die Erstellung von Passwörtern.

## Wie man Zufallszahlen generiert

Die Generierung von Zufallszahlen ist in Javascript recht einfach. Dafür gibt es die eingebaute Methode `Math.random()`, die eine zufällige Dezimalzahl zwischen 0 und 1 zurückgibt.

```Javascript
// Beispiel für die Generierung von einer Zufallszahl zwischen 0 und 10
let randomNum = Math.random() * 10;
console.log(randomNum); // Output kann z. B. 6.73851 sein
```

Um eine ganze Zahl zu erhalten, kann man die Methode `Math.floor()` verwenden, die die erste Zahl vor dem Komma nimmt.

```Javascript
// Beispiel für die Generierung einer Zufallszahl zwischen 1 und 100
let randomNum = Math.floor(Math.random() * 100) + 1;
console.log(randomNum); // Output kann z. B. 65 sein
```

Man kann auch eine benutzerdefinierte Funktion erstellen, um Zufallszahlen in einem bestimmten Bereich zu generieren.

```Javascript
// Beispiel für eine Funktion zum Generieren von Zufallszahlen zwischen 5 und 15
function randomNumBetween(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(randomNumBetween(5, 15)); // Output kann z. B. 9 sein
```

## Tiefergehende Informationen zur Generierung von Zufallszahlen

Die Wahl eines Seed, also eines Ausgangspunkts, für die Generierung von Zufallszahlen kann eine wichtige Rolle spielen. Wenn kein Seed verwendet wird, wird standardmäßig der aktuelle Zeitstempel verwendet, was dazu führen kann, dass die gleichen Zahlenfolgen bei jedem Aufruf der Methode `Math.random()` zurückgegeben werden. Mit einem manuell gewählten Seed kann man eine eindeutige Sequenz von Zahlen generieren.

## Siehe auch

- Einführung in die Javascript-Mathematik: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math
- Erstellung von Passwörtern mit zufälligen Zahlen: https://www.w3schools.com/js/js_random.asp