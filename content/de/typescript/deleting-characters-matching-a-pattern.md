---
title:                "TypeScript: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung kommt es oft vor, dass man bestimmte Zeichen aus einem String entfernen muss, die einem bestimmten Muster entsprechen. Dies kann zum Beispiel bei der Verarbeitung von Benutzereingaben oder beim Filtern von Daten hilfreich sein. In diesem Blog-Beitrag zeige ich euch, wie man in TypeScript Zeichen löschen kann, die einem bestimmten Muster entsprechen.

## Wie geht's

Um Zeichen in TypeScript zu löschen, die einem Muster entsprechen, können wir die String-Methode `replace()` verwenden. Diese Methode nimmt zwei Parameter entgegen: das zu ersetzende Muster und den Ersatztext.

```TypeScript
let string = "Hallo Welt! Hallo TypeScript!";
let newString = string.replace(/Hallo/g, "Hi");

console.log(newString); // Hi Welt! Hi TypeScript!
```

In diesem Beispiel benutzen wir die String-Methode `replace()` zusammen mit dem regulären Ausdruck `/Hallo/g`, um alle Vorkommnisse von "Hallo" im String zu ersetzen. Der zweite Parameter ist in diesem Fall einfach der ersetzende Text "Hi". Die Methode `replace()` gibt einen neuen String zurück, während die ursprüngliche Variable `string` unverändert bleibt.

Natürlich können wir auch komplexere reguläre Ausdrücke verwenden, um bestimmte Zeichen oder Muster zu löschen. Zum Beispiel können wir mit dem Platzhalter `.` alle Zeichen löschen oder mit `[0-9]` alle Zahlen. Hier nur ein kleines Beispiel:

```TypeScript
let string = "Mai 2021";
let newString = string.replace(/[0-9]/g, "");

console.log(newString); // Mai
```

In diesem Beispiel löschen wir alle Zahlen aus dem String "Mai 2021", indem wir den regulären Ausdruck `[0-9]` verwenden. Die Methode `replace()` ersetzt diese Zahlen dann durch einen leeren String, sodass nur noch der Monat "Mai" übrig bleibt.

## Tiefergehende Erklärung

Die String-Methode `replace()` verwendet reguläre Ausdrücke, um zu bestimmen, welche Zeichen oder Muster ersetzt werden sollen. Reguläre Ausdrücke sind eine mächtige Sprache, um Textmuster zu beschreiben und zu suchen. Sie bestehen aus verschiedenen Zeichen und Symbolen, die verschiedene Muster repräsentieren, zum Beispiel `.` für ein beliebiges Zeichen oder `[]` für eine Liste von Zeichen. Hier findet ihr eine ausführliche Erklärung zu regulären Ausdrücken in TypeScript: [Link 1](https://www.regular-expressions.info/javascript.html) und [Link 2](https://www.typescriptlang.org/docs/handbook/regular-expressions.html).

## Siehe auch

- [String-Methode `replace()`](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Reguläre Ausdrücke in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html#regular-expression-support)
- [Eine interaktive Plattform zum Üben von regulären Ausdrücken](https://regex101.com/)