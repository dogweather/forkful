---
title:    "Javascript: Ausgabe von Debugging-Daten"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wesentlicher Bestandteil der Programmierung in Javascript. Sie ermöglichen es Entwicklern, den Programmablauf zu verfolgen und Fehler zu erkennen, was letztendlich zu einer besseren Codequalität führt.

## Wie es geht

Um Debug-Ausgaben zu erstellen, verwenden wir die Javascript-Funktion `console.log()`. Diese Funktion akzeptiert eine beliebige Anzahl von Argumenten, die im Konsolenfenster angezeigt werden.

```Javascript
let num1 = 5;
let num2 = 10;

console.log("Die Summe von", num1, "und", num2, "ist", num1 + num2);
```

Dieses Beispiel würde folgende Ausgabe erzeugen:

```
Die Summe von 5 und 10 ist 15
```

Wir können auch Variablen, Datenstrukturen und sogar Funktionen in der `console.log()`-Funktion verwenden, um den Wert und den Inhalt zu überprüfen. Dies ist besonders hilfreich, wenn unser Programm nicht das erwartete Ergebnis liefert.

```Javascript
let arr = ["Apfel", "Birne", "Banane", "Orange"];

console.log("Das erste Element im Array ist", arr[0]);

function sayHello(name) {
  console.log("Hallo", name);
}

sayHello("Max");
```

Dies würde folgende Ausgabe generieren:

```
Das erste Element im Array ist Apfel
Hallo Max
```

## Tiefere Einblicke

Es gibt mehrere Möglichkeiten, Debug-Ausgaben zu verwenden, um effektiver zu debuggen. Eine davon ist die Verwendung von bedingten Ausgaben, bei denen die Konsolenausgabe nur angezeigt wird, wenn eine bestimmte Bedingung erfüllt ist.

```Javascript
let num1 = 5;

console.log("Die Variable num1 wurde deklariert");

if (num1 < 10) {
  console.log("Der Wert von num1 ist kleiner als 10");
}
```

In diesem Beispiel wird die zweite Debug-Ausgabe nur angezeigt, wenn `num1` kleiner als 10 ist. Dadurch können wir genauere Informationen über unseren Code erhalten und möglicherweise versteckte Fehler leichter finden.

Eine weitere Möglichkeit, Debug-Ausgaben zu verwenden, ist die Verwendung von Zeitmessungen. Mit `console.time()` und `console.timeEnd()` können wir den Zeitpunkt zwischen zwei Codeabschnitten messen und feststellen, wie lange der Programmdurchlauf insgesamt dauert.

```Javascript
console.time("Programmdurchlauf");

// Führe komplexe Aktionen aus

console.timeEnd("Programmdurchlauf");
```

Die Ausgabe sieht dann zum Beispiel so aus:

```
Programmdurchlauf: 1.234 ms
```

Dies kann uns helfen, zeitkritische Bereiche in unserem Code zu identifizieren und zu optimieren.

## Siehe auch

- [MDN Web Docs: console.log()](https://developer.mozilla.org/de/docs/Web/API/Console/log)
- [JavaScript Debugging for Beginners](https://blog.bitsrc.io/javascript-debugging-for-beginners-f149a3541b45)
- [5 JavaScript Debugging Tips You'll Regret Not Knowing Earlier](https://medium.com/@tomsu/javascript-debugging-tips-youll-regret-not-knowing-8a2431b90e2)