---
title:    "Javascript: Großschreibung eines Zeichens"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Warum

Das Verketten von Buchstaben in einer richtigen Groß- und Kleinschreibung kann für viele Entwickler eine Routinetätigkeit sein. Aber warum sollte man sich überhaupt damit beschäftigen? In diesem Blogbeitrag werden wir uns genauer damit beschäftigen, warum es wichtig ist, Strings in der richtigen Schreibweise zu formatieren.

# Wie man Strings in Javascript großschreibt

Um einen String in Javascript großzuschreiben, gibt es verschiedene Möglichkeiten. Hier sind zwei Beispiele:

```Javascript
// Methode 1: String.toUpperCase()
let str1 = "hallo welt";
let upperStr1 = str1.toUpperCase();

console.log(upperStr1); // Ausgabe: HALLO WELT
```

```Javascript
// Methode 2: for-Schleife
let str2 = "hallo welt";
let upperStr2 = "";

for (let i = 0; i < str2.length; i++) {
    upperStr2 += str2[i].toUpperCase();
}

console.log(upperStr2); // Ausgabe: HALLO WELT
```

# Tiefgehende Information über das Verketten von Strings

Das Formatieren von Strings in der richtigen Schreibweise ist nicht nur wichtig für die Lesbarkeit des Codes, sondern auch für die Ausgabe von Daten für den Benutzer. Zum Beispiel kann eine schlechte Schreibweise in einer E-Mail-Adresse dazu führen, dass die E-Mail nicht zugestellt werden kann.

Eine tiefergehende Betrachtung der String-Manipulation in Javascript umfasst auch die Verwendung von regulären Ausdrücken (RegExp) für komplexere Formatierungen.

# Siehe auch

- [MDN Dokumentation zu String-Methoden](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Wie man reguläre Ausdrücke in Javascript verwendet](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)