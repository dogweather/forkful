---
title:    "TypeScript: Verwendung von regulären Ausdrücken"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Funfact: Wussten Sie, dass reguläre Ausdrücke schon seit den 50er Jahren existieren? Trotz ihres Alters sind sie immer noch eine wertvolle und oft genutzte Funktion in der Programmierung. Mit regulären Ausdrücken können Sie Textmuster in Strings erkennen und verarbeiten. Das ist besonders hilfreich, wenn Sie z.B. Daten aus einer großen Menge an Text extrahieren oder Validierungen durchführen möchten. In diesem Blog-Post erfahren Sie, wie Sie reguläre Ausdrücke in Ihrer TypeScript-Programmierung einsetzen können.

## Wie

Um reguläre Ausdrücke in TypeScript zu verwenden, müssen Sie zuerst das `RegExp` Objekt erstellen. Hier ein einfaches Beispiel, wie Sie eine Zeichenkette auf ein bestimmtes Muster überprüfen können:

```TypeScript
let string = "Hallo Welt";
let regex = new RegExp("Welt");

console.log(regex.test(string)); // Output: true
```

In diesem Beispiel verwenden wir die `test()` Methode, um zu prüfen, ob unsere Zeichenkette den regulären Ausdruck `"Welt"` enthält. Als Ergebnis erhalten wir `true`, da das Muster in der Zeichenkette enthalten ist. Aber wie können wir jetzt bestimmte Teile des Textes extrahieren?

Dafür gibt es die `exec()` Methode:

```TypeScript
let string = "Hallo Welt";
let regex = new RegExp("Welt");

console.log(regex.exec(string)); // Output: ["Welt", index: 6, input: "Hallo Welt"]
```

Diesmal erhalten wir einen Array zurück, der das gefundene Muster, den Index und den gesamten Eingabetext enthält. Im diesem Fall ist das gefundene Muster `"Welt"` und befindet sich an der 6. Stelle im Text.

Natürlich können reguläre Ausdrücke auch viel komplexere Muster erkennen, z.B. IP-Adressen, E-Mail Adressen oder Telefonnummern. Hier ist ein Beispiel, wie Sie eine E-Mail Adresse aus einem Text extrahieren können:

```TypeScript
let string = "Senden Sie mir bitte eine E-Mail an: info@muster.de";
let regex = new RegExp(/\w+@\w+\.\w+/);

console.log(regex.exec(string)); // Output: ["info@muster.de"]
```

## Deep Dive

Jetzt, da Sie wissen, wie Sie reguläre Ausdrücke in TypeScript verwenden können, gibt es noch einige Funktionen und Besonderheiten, die es zu beachten gilt. Zum Beispiel können Sie mit der `g` Flagge mehrere Vorkommen des Musters in einem Text erkennen:

```TypeScript
let string = "Sie können mich unter 123-456-789 oder 246-810-1210 erreichen.";
let regex = new RegExp(/\d{3}-\d{3}-\d{3}/g);

console.log(regex.exec(string)); // Output: ["123-456-789"]
console.log(regex.exec(string)); // Output: ["246-810-1210"]
```
In diesem Beispiel erkennen wir beide Telefonnummern in der Zeichenkette, da die `g` Flagge angegeben ist. Außerdem gibt es auch noch die `i` Flagge, mit der Sie Ihre Suche case-insensitive machen können.

Eine weitere nützliche Funktion von regulären Ausdrücken in TypeScript ist die Möglichkeit, Gruppen von Zeichen innerhalb eines Musters zu kennzeichnen und diese gesondert zu bearbeiten. Hier ein Beispiel, wie Sie Vor- und Nachnamen aus einer Zeichenkette extrahieren können:

```TypeScript
let string = "Max Mustermann";
let regex = new RegExp(/(\w+)\s(\w+)/);

let [match, firstName, lastName] = regex.exec(string);
console.log(`Vorname: ${firstName} | Nachname: ${lastName}`); // Output: Vorname: Max | Nachname: Mustermann
```

Hier verwenden wir die `exec()` Methode in Kombination mit Destructuring, um die gefundenen Namen in separate Variablen zu speichern.

## Siehe auch

- [Offizielle TypeScript Dokumentation zu regulären Ausdrücken](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Online Tool zum Testen von regulären Ausdrücken in TypeScript](https://regex101.com/)
- [Tutorial zu regulären Ausdrücken in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)

Vielen Dank fürs Lesen! Wir