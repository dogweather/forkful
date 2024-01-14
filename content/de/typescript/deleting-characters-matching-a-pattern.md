---
title:    "TypeScript: Löschen von Zeichen, die einem Muster entsprechen."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung sehr hilfreich sein, um unerwünschte Zeichen oder Daten zu entfernen. Zum Beispiel kann dies nützlich sein, um Eingaben von Benutzern zu überprüfen oder um Daten in einem bestimmten Format zu halten.

## Wie geht man vor

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, kann man die `replace()` Methode in TypeScript verwenden. Diese Methode ermöglicht es, einen regulären Ausdruck als erstes Argument zu übergeben, um zu definieren, welche Zeichen ersetzt werden sollen. Als zweites Argument gibt man dann den Ersatztext an, der anstelle der übereinstimmenden Zeichen eingefügt werden soll. Hier ist ein Beispiel:

```TypeScript
const string = "Hello World!";
const pattern = /[eo]/g;
const newString = string.replace(pattern, "");

console.log(newString); // Hll Wrld!
```

In diesem Beispiel wird der reguläre Ausdruck `/[eo]/g` verwendet, um alle Vorkommen der Zeichen "e" und "o" im String "Hello World!" zu löschen. Der zweite Parameter ist ein leerer String `""`, was bedeutet, dass die übereinstimmenden Zeichen einfach durch nichts ersetzt werden.

## Tiefergehende Informationen

Die `replace()` Methode akzeptiert auch eine Funktion als zweites Argument, anstatt eines einfachen Ersatztexts. Diese Funktion wird für jede Übereinstimmung aufgerufen und erlaubt es, den Ersatztext basierend auf der Übereinstimmung individuell zu generieren. Hier ist ein Beispiel:

```TypeScript
const string = "Hello World!";
const pattern = /[eo]/g;
const newString = string.replace(pattern, (match, offset, fullString) => {
  if (match === "e") {
    return "a";
  }
  if (match === "o") {
    return "u";
  }
});

console.log(newString); // Hallo Warld!
```

In diesem Beispiel wird die Funktion verwendet, um für jede Übereinstimmung einen individuellen Ersatztext zu generieren. So wird das "e" durch ein "a" und das "o" durch ein "u" ersetzt.

## Siehe auch

- [Dokumentation der replace() Methode in der offiziellen TypeScript Dokumentation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-replace)
- [Tutorial zu regulären Ausdrücken in TypeScript](https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm)