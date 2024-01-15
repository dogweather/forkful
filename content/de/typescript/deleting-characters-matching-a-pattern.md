---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "TypeScript: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man gezielt bestimmte Zeichen aus einer Zeichenkette löschen möchte. Beispielsweise können unerwünschte Leerzeichen oder Sonderzeichen entfernt werden, um saubere und einheitliche Daten zu erhalten. Auch bei der Validierung von Eingaben oder beim Parsen von Strings kann es notwendig sein, bestimmte Zeichen zu entfernen.

## Wie geht man vor?

Um Zeichen in einer Zeichenkette zu löschen, können wir die replace() Methode von JavaScript verwenden. Sie erwartet zwei Parameter: das zu ersetzende Zeichen und das gewünschte Ersetzungsmuster.

```TypeScript
const str = "Hallo, Welt!";

// Entferne alle Kommas aus der Zeichenkette
const newStr = str.replace(",", "");

console.log(newStr); // Ausgabe: Hallo Welt!
```

Das Ersetzungsmuster kann auch ein regulärer Ausdruck sein, der es uns ermöglicht, gezielt nach bestimmten Mustern zu suchen. Zum Beispiel können wir mit dem regulären Ausdruck /[0-9]/ alle Zahlen aus einer Zeichenkette löschen.

```TypeScript
const str = "1a2b3c4d5e";

// Entferne alle Zahlen aus der Zeichenkette
const newStr = str.replace(/[0-9]/g, "");

console.log(newStr); // Ausgabe: abced
```

## Tiefergehende Informationen

Regex (Reguläre Ausdrücke) bieten noch viele weitere Möglichkeiten, um Zeichen aus einer Zeichenkette zu entfernen. Man kann zum Beispiel angeben, dass nur Zahlen oder Buchstaben gelöscht werden sollen oder dass nur bestimmte Zeichenkombinationen entfernt werden sollen.

Zusätzlich gibt es noch die Möglichkeit, die replace() Methode mit einer Funktion zu verwenden. Diese Funktion wird für jedes gefundene Zeichen aufgerufen und erwartet die Übereinstimmung (match) sowie den Index des ersten Zeichens. Auf diese Weise können wir noch komplexere Logik implementieren, um genau die Zeichen zu löschen, die wir wollen.

## Siehe auch

- [MDN Web Docs: replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [regex101: Online Regex Tester](https://regex101.com)
- [TypeScript Leitfaden](https://www.typescriptlang.org/docs/handbook/intro.html)