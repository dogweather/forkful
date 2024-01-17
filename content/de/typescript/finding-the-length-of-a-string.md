---
title:                "Die Länge eines Strings finden"
html_title:           "TypeScript: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was ist die Länge einer Zeichenkette und warum brauchen Programmierer sie?

Die Länge einer Zeichenkette bezieht sich auf die Anzahl der Zeichen oder Buchstaben, aus denen eine Zeichenkette besteht. Programmierer benötigen diese Information, um beispielsweise festzustellen, ob eine Eingabe den Anforderungen entspricht oder um Zeichenketten effektiv zu verarbeiten.

## Wie geht man vor:

```TypeScript
// Beispiel:
const str = "Hallo, Welt!";
console.log(str.length);
```

Dieses Beispiel zeigt, wie man die Länge einer Zeichenkette in TypeScript ermittelt. Zunächst wird die Zeichenkette in einer Variablen gespeichert, in diesem Fall "str". Anschließend wird die "length"-Eigenschaft aufgerufen, die die Anzahl der Zeichen der Zeichenkette zurückgibt. In diesem Fall würde die Ausgabe 13 sein.

## Tiefendinformationen:

Die Möglichkeit, die Länge einer Zeichenkette zu ermitteln, existiert schon seit den ersten Programmiersprachen wie C und Java. In TypeScript kann diese Eigenschaft auch auf Arrays angewendet werden, um die Anzahl der enthaltenen Elemente zu ermitteln.

Alternativ gibt es auch die Methode "string.length()", die ebenfalls die Länge einer Zeichenkette zurückgibt. Diese Methode ist jedoch nicht für Array-Objekte verfügbar.

Die Implementierung der Lösung ist abhängig von der Programmiersprache. In TypeScript wird die "length"-Eigenschaft durch eine Getter-Methode berechnet, die bei jedem Aufruf die Anzahl der Zeichen oder Elemente zurückgibt.

## Weitere Informationen:

Weitere Informationen zur Länge von Zeichenketten in TypeScript findet man in der offiziellen Dokumentation von Microsoft: https://www.typescriptlang.org/docs/handbook/basic-types.html#string.

Eine umfassendere Erklärung, wie die Länge von Zeichenketten in TypeScript funktioniert, ist in diesem Artikel zu finden: https://www.freecodecamp.org/news/how-to-find-the-length-of-a-string-in-typescript-78fd161a4a56/.