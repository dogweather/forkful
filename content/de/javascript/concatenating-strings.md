---
title:    "Javascript: Verkettung von Zeichenfolgen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum:

Als Entwickler*innen sind wir immer auf der Suche nach Möglichkeiten, unsere Programme effizienter und eleganter zu gestalten. Eine grundlegende Programmierkonzept, das uns dabei helfen kann, ist die Verkettung von Strings.

## Wie geht das?

Mit JavaScript können wir ganz einfach Strings miteinander verbinden, indem wir das Pluszeichen (+) verwenden. Hier ist ein Beispiel, wie wir den Vornamen und Nachnamen einer Person zu einem vollen Namen verketten können:

```Javascript
let vorname = "Max";
let nachname = "Mustermann";
let vollerName = vorname + " " + nachname;
console.log(vollerName);
// Ausgabe: Max Mustermann
```

Wir können auch Variablen und Strings miteinander verbinden, um dynamische Strings zu erstellen. Hier ist ein Beispiel, wie wir die Zahlen 1 und 2 zu einem String zusammenfassen können:

```Javascript
let zahl1 = 1;
let zahl2 = 2;
let summe = zahl1 + " + " + zahl2 + " = " + (zahl1 + zahl2);
console.log(summe);
// Ausgabe: 1 + 2 = 3
```

## Tiefer gehen:

Bei der Verkettung von Strings in JavaScript gibt es ein paar wichtige Dinge zu beachten. Zum Beispiel können wir keine Strings und Zahlen direkt verketten, wir müssen sie zuerst mit der Funktion `toString()` in Strings umwandeln. Außerdem können wir auch die Methode `concat()` verwenden, um Strings anstatt des Pluszeichens zu verketten. Und schließlich sollten wir immer darauf achten, unsere Strings sorgfältig zu formatieren, damit sie lesbar und verständlich bleiben.

## Siehe auch:

- [MDN Web Docs: String concatenation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/concat)

- [GeeksforGeeks: Concatenation of strings in JavaScript](https://www.geeksforgeeks.org/concatenation-of-strings-in-javascript/)

- [W3Schools: JavaScript String concatenation](https://www.w3schools.com/jsref/jsref_concat_string.asp)