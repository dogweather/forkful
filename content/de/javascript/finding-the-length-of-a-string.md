---
title:    "Javascript: Die Länge eines Strings ermitteln"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Länge einer Zeichenkette in Javascript zu finden, mag auf den ersten Blick wie eine einfache Aufgabe erscheinen. Doch es ist eine grundlegende Funktion, die in vielen Programmiersprachen verwendet wird und auch für Javascript-Entwickler von großer Bedeutung ist. Die Länge einer Zeichenkette zu kennen, kann dabei helfen, bestimmte Operationen auszuführen oder Bedingungen zu überprüfen. In diesem Artikel werde ich dir zeigen, wie du mithilfe von Javascript die Länge einer Zeichenkette finden kannst und warum dies hilfreich sein kann.

# Wie

Um die Länge einer Zeichenkette in Javascript zu finden, gibt es mehrere Methoden. Eine der einfachsten Möglichkeiten ist die Verwendung der `length`-Eigenschaft. Diese Eigenschaft gibt die Anzahl der Zeichen in einer Zeichenkette zurück.

```
// Beispiel 1
const zeichenkette = "Hallo Welt";
console.log(zeichenkette.length); // Ausgabe: 11

// Beispiel 2
const leereZeichenkette = "";
console.log(leereZeichenkette.length); // Ausgabe: 0
```

Eine andere Methode ist die Verwendung der `substring`-Methode. Diese Methode gibt einen Teil der Zeichenkette zurück, abhängig von den angegebenen Start- und Endpositionen. Wenn du jedoch nur die Länge der Zeichenkette benötigst, kannst du die Startposition auf 0 setzen und die Länge der Zeichenkette als Endposition verwenden.

```
// Beispiel
const zeichenkette = "Hello World";
const laenge = zeichenkette.substring(0, zeichenkette.length);
console.log(laenge); // Ausgabe: 11
```

Eine weitere Option ist die Verwendung der `split`-Methode, die die Zeichenkette in ein Array umwandelt und dann die Länge des Arrays zurückgibt.

```
// Beispiel
const zeichenkette = "JavaScript ist toll!";
const laenge = zeichenkette.split("").length;
console.log(laenge); // Ausgabe: 20
```

# Deep Dive

Bei der Suche nach der Länge einer Zeichenkette gibt es einige wichtige Dinge zu beachten. Eine Zeichenkette kann nicht nur aus Buchstaben bestehen, sondern auch aus anderen Zeichen wie Leerzeichen, Zahlen oder Sonderzeichen. Diese werden alle bei der Ermittlung der Länge berücksichtigt. Auch Umlaute oder nicht lateinische Zeichen können die Länge einer Zeichenkette beeinflussen, da sie aus mehreren Bytes bestehen können.

Es ist auch wichtig zu beachten, dass die `length`-Eigenschaft die Anzahl der Zeichen in der Zeichenkette und nicht die Anzahl der Wörter zurückgibt. Dies bedeutet, dass Leerzeichen und Satzzeichen ebenfalls in die Länge einbezogen werden.

# Siehe auch

- [MDN web docs: String length](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools: Javascript string length](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Javascript.info: Unterstriche, um Zahlen lesbar zu machen](https://javascript.info/advanced-operators#ufeff-to-make-numbers-readable)