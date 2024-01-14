---
title:    "TypeScript: Umwandeln einer Zeichenkette in Kleinbuchstaben"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben kann in vielen unterschiedlichen Programmierszenarien nützlich sein. Oftmals ist es notwendig, Eingaben des Benutzers oder Daten aus einer Datenbank zu normalisieren, um eine einheitliche Verarbeitung zu gewährleisten. Die Verwendung von Kleinbuchstaben kann auch bei Vergleichen und Sortierungen von Strings hilfreich sein.

## Wie man es macht

Um einen String in TypeScript in Kleinbuchstaben zu konvertieren, kann die integrierte Methode `toLowerCase()` verwendet werden. Diese Methode gibt eine neue Zeichenkette zurück und ändert nicht den ursprünglichen String.

```TypeScript
let string = "Guten Tag!";
console.log(string.toLowerCase()); // Output: guten tag!
```

Falls der ursprüngliche String verändert werden soll, kann stattdessen die Methode `toLocaleLowerCase()` verwendet werden. Diese ändert den String selbst.

```TypeScript
let string = "Guten Tag!";
string = string.toLocaleLowerCase();
console.log(string); // Output: guten tag! 
```

## Tiefgehende Einblicke

Bei der Konvertierung in Kleinbuchstaben werden die Zeichen basierend auf der aktuellen Sprache des Systems in den entsprechenden Kleinbuchstaben umgewandelt. Dies kann in manchen Fällen zu unerwarteten Ergebnissen führen, insbesondere bei Zeichen, die in verschiedenen Sprachen unterschiedlich sind, wie zum Beispiel 'I' und 'i'. Es ist daher wichtig, immer die Verwendung von `toLocaleLowerCase()` zu überprüfen und gegebenenfalls die Sprache zu definieren.

Wichtig ist auch zu beachten, dass bei Unicode-Zeichen, die aus mehr als einem Zeichen bestehen, nur das erste Zeichen in Kleinbuchstaben umgewandelt wird. Um alle Zeichen in Kleinbuchstaben zu konvertieren, kann die Normungsfunktion `NFC` verwendet werden.

## Siehe auch

- [Microsoft TypeScript Dokumentation zu toLowerCase()](https://www.typescriptlang.org/docs/handbook/utility-types.html#lowercasestring) 
- [Stapelüberlauf: Unterschied zwischen toLocaleLowerCase und toLowerCase](https://stackoverflow.com/questions/33078853/difference-between-tolocalelowercase-and-tolowercase-in-javascript)