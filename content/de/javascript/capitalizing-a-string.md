---
title:    "Javascript: Eine Zeichenkette großschreiben"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Großschreiben von Strings ist eine häufige Aufgabe beim Programmieren. Es kann dazu dienen, die Lesbarkeit von Texten oder Variablennamen zu verbessern oder bestimmte Anforderungen von APIs oder Datenbanken zu erfüllen. Auch bei der Validierung von Eingaben oder der Formatierung von Benutzereingaben kann das Großschreiben von Strings nützlich sein.

## Wie geht man vor

Um einen String in Javascript zu großzuschreiben, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der `toUpperCase()` Funktion, die den gesamten String in Großbuchstaben umwandelt.

```Javascript
const text = "dies ist ein beispiel";
const capitalizedText = text.toUpperCase();

console.log(capitalizedText); // Ausgabe: "DIES IST EIN BEISPIEL"
```

Eine weitere Möglichkeit ist die Verwendung von Regular Expressions. Hier wird der `replace()` Befehl verwendet, um alle Kleinbuchstaben durch die entsprechenden Großbuchstaben zu ersetzen.

```Javascript
const text = "dies ist ein beispiel";
const capitalizedText = text.replace(/[a-z]/g, letter => letter.toUpperCase());

console.log(capitalizedText); // Ausgabe: "DIES IST EIN BEISPIEL"
```

Für speziellere Anforderungen, z.B. um nur den ersten Buchstaben eines Strings großzuschreiben, gibt es auch spezielle Funktionen wie `charAt()` und `slice()`.

## Tiefergehende Informationen

Das Großschreiben von Strings kann auch einige Herausforderungen mit sich bringen, z.B. bei der Behandlung von Sonderzeichen oder verschiedenen Sprach- und Schreibweisen. Es ist daher wichtig, die verwendete Methode genau zu verstehen und gegebenenfalls anzupassen.

## Siehe auch

- [MDN Dokumentation zur `toUpperCase()` Funktion](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Dokumentation zur `replace()` Funktion](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Dokumentation zur `charAt()` Funktion](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Dokumentation zur `slice()` Funktion](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/slice)