---
title:                "TypeScript: Unterstrings extrahieren."
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

Das Extrahieren von Teilstrings ist eine nützliche Fähigkeit für jeden TypeScript-Programmierer. Es ermöglicht uns, Teilstrings aus einer längeren Zeichenkette zu extrahieren, um spezifische Informationen zu erhalten. Dies kann hilfreich sein, um z.B. Benutzernamen oder E-Mails aus einer Eingabe zu gewinnen.

# Wie geht das?

```TypeScript
// Beispiel einer Zeichenkette
let text = "Willkommen bei meinem TypeScript-Blog!"

// Extrahieren des Teilstrings "TypeScript"
let substr = text.substring(14, 23);

console.log(substr); // Ausgabe: TypeScript
```

In diesem Beispiel definieren wir eine Zeichenkette und verwenden dann die `substring()`-Funktion, um einen Teil von ihr zu extrahieren. Wir geben den Startindex (14) und die Länge des Teilstrings (9) an. Das Ergebnis wird in der Variablen `substr` gespeichert und anschließend ausgegeben.

# Tiefentauchen

Es gibt noch mehr, was wir mit der `substring()`-Funktion tun können. Wir können auch angeben, dass der Teilstring bis zum Ende der Zeichenkette extrahiert werden soll, indem wir nur den Startindex angeben. Außerdem können wir auch negative Zahlen verwenden, um von rechts zu zählen.

```TypeScript
// Beispiel einer Zeichenkette
let text = "Das ist ein Beispieltext";

// Extrahieren des Teilstrings "Beispieltext"
let substr = text.substring(12); 

console.log(substr); // Ausgabe: Beispieltext

// Extrahieren des Teilstrings "ist ein Beispiel"
let substr2 = text.substring(-7, 16); 

console.log(substr2); // Ausgabe: ist ein Beispiel
```

Es ist auch wichtig zu beachten, dass die Funktion `substring()` immer einen neuen Teilstring zurückgibt, ohne die ursprüngliche Zeichenkette zu ändern.

# Siehe auch

- [MDN Web Docs zu substring()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools über substring()](https://www.w3schools.com/jsref/jsref_substring.asp)
- [TypeScript Dokumentation zu Zeichenkettenmanipulation](https://www.typescriptlang.org/docs/handbook/strings.html)