---
title:    "Javascript: Fehlerausgabe drucken"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wichtiges Werkzeug beim Programmieren, um Fehler zu finden und zu beheben. Mit ihnen können wir den Programmablauf verfolgen und überprüfen, ob unsere Variablen und Funktionen korrekt arbeiten.

## How To

Um Debug-Ausgaben in Javascript zu verwenden, können wir die eingebaute Funktion "console.log()" verwenden. Diese Funktion nimmt einen oder mehrere Parameter entgegen und gibt diese in der Konsole aus. Hier ist ein einfaches Beispiel:

```Javascript
var num1 = 10;
var num2 = 5;
console.log(num1 + num2);
```

Dieser Code würde "15" in der Konsole ausgeben. Wir können auch Zeichenketten und Variablen in der Ausgabe kombinieren, zum Beispiel:

```Javascript
var name = "Max";
var age = 25;
console.log("Mein Name ist " + name + " und ich bin " + age + " Jahre alt.");
```

Dies würde die folgende Ausgabe erzeugen: "Mein Name ist Max und ich bin 25 Jahre alt.".

## Deep Dive

Neben "console.log()" gibt es auch andere Funktionen, die uns beim Debuggen helfen können, wie zum Beispiel "console.error()" für Fehlerausgaben oder "console.warn()" für Warnungen. Eine weitere nützliche Funktion ist "console.table()", mit der wir komplexe Datenstrukturen wie Arrays oder Objekte übersichtlich in einer Tabelle ausgeben können.

Es ist auch möglich, Bedingungen in unsere Debug-Ausgaben einzubauen, um zu kontrollieren, ob bestimmte Teile unseres Codes erreicht werden. Hier ist ein Beispiel mit "console.assert()":

```Javascript
var num = 5;
console.assert(num >= 10, "Die Zahl ist nicht größer oder gleich 10.");
```

Wenn "num" kleiner als 10 ist, wird die Fehlermeldung in der Konsole ausgegeben. Andernfalls passiert nichts.

## Siehe auch

- https://developer.mozilla.org/de/docs/Web/API/console
- https://www.w3schools.com/js/js_debugging.asp
- https://www.digitalocean.com/community/tutorials/how-to-debug-javascript-with-chrome-devtools