---
title:                "TypeScript: Umwandeln einer Zeichenkette in Kleinbuchstaben"
simple_title:         "Umwandeln einer Zeichenkette in Kleinbuchstaben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Umwandeln eines Strings in Kleinbuchstaben kann in verschiedenen Situationen nützlich sein, z.B. um die Eingabe eines Benutzers zu standardisieren oder bestimmte Textvergleiche durchzuführen.

## Wie geht man vor

Ganz einfach: Verwende die built-in Funktion `toLowerCase()`!

```TypeScript
let string = "Hallo WELT";
console.log(string.toLowerCase());
```

Dies wird die Ausgabe `'hallo welt'` zurückgeben.

## Tiefere Einblicke

Bei der Verwendung von `toLowerCase()` gibt es ein paar Dinge zu beachten. Zum einen wird der ursprüngliche String nicht verändert, sondern eine neue Kopie zurückgegeben. Dies bedeutet, dass wenn man den ursprünglichen String beibehalten möchte, man ihn in einer neuen Variable speichern muss. Zum anderen ist es wichtig zu wissen, dass diese Funktion nur mit String-Objekten funktioniert. Wenn also z.B. eine Zahl eingegeben wird, wird ein Fehler ausgegeben.

## Siehe auch

- [`toUpperCase()` in TypeScript](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#uppercase-literal-types)
- [String manipulation in TypeScript](https://mariusschulz.com/blog/string-manipulation-in-typescript)