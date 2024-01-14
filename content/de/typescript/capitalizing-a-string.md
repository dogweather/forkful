---
title:    "TypeScript: Großschreibung einer Zeichenkette"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Eine häufige Aufgabe beim Programmieren ist es, Strings in bestimmten Fällen groß zu schreiben. Dies kann zum Beispiel nützlich sein, um Benutzereingaben zu validieren oder um ein einheitliches Ausgabeformat zu gewährleisten.

## So geht's

Die einfachste Möglichkeit, einen String in TypeScript groß zu schreiben, ist die Verwendung der integrierten Methode `toUpperCase()`. Diese Methode wandelt alle Buchstaben in Großbuchstaben um. Hier ist ein Beispiel:

```TypeScript
let name = "max mustermann";
console.log(name.toUpperCase()); // Ausgabe: MAX MUSTERMANN
```
Eine weitere Möglichkeit ist die Verwendung von Regular Expressions. In der folgenden Funktion wird der erste Buchstabe eines Strings großgeschrieben, während alle weiteren Buchstaben klein bleiben:

```TypeScript
function capitalizeString(str: string) {
    return str.replace(/\b\w/g, (l) => l.toUpperCase());
}

console.log(capitalizeString("hallo welt")); // Ausgabe: Hallo Welt
```

## Tiefergehende Informationen

Die `toUpperCase()` Methode funktioniert nur mit lateinischen Buchstaben. Wenn Sie jedoch mit mehrsprachigen Strings arbeiten, kann das Ergebnis möglicherweise nicht das gewünschte sein. In diesem Fall bietet die `toLocaleUpperCase()` Methode eine Lösung. Diese Methode berücksichtigt sprachliche Besonderheiten und wandelt den String entsprechend um.

Eine weitere wichtige Sache, die beachtet werden sollte, ist, dass Strings in TypeScript unveränderlich sind. Das bedeutet, dass Sie nicht einfach eine Methode auf einen String anwenden können, um diesen dauerhaft zu ändern. Stattdessen müssen Sie das Ergebnis der Methode einer neuen Variablen zuweisen. Zum Beispiel:

```TypeScript
let name = "max mustermann";
let upperName = name.toUpperCase(); // neue Variable wird deklariert und Wert zugewiesen
console.log(name); // Ausgabe: max mustermann
console.log(upperName); // Ausgabe: MAX MUSTERMANN
```

## Siehe auch

- [TypeScript String Dokumentation](https://www.typescriptlang.org/docs/handbook/2/strings.html)
- [Regular Expressions in TypeScript](https://www.javascripttutorial.net/typescript/typescript-regular-expression/)
- [String Manipulation in TypeScript](https://www.geeksforgeeks.org/string-manipulation-in-typescript/)