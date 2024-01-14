---
title:                "TypeScript: Zeichenfolgen verketten"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Strings ist ein grundlegender Prozess in der Programmierung, bei dem mehrere Strings zu einem einzigen neuen String kombiniert werden. Dies ist besonders nützlich bei der Erstellung von dynamischen Texten, z.B. bei der Ausgabe von Benutzerdaten oder bei der Implementierung von Suchfunktionen.

## Wie man Strings verbindet

Um Strings in TypeScript zu verketten, können wir den `+` Operator verwenden. Hier ist ein Beispielcode, der zwei Strings miteinander verbindet und das Ergebnis in der Konsole ausgibt:

```TypeScript
let str1 = "Hallo, ";
let str2 = "Welt!";
console.log(str1 + str2); // Ausgabe: Hallo, Welt!
```

Wir können auch die `concat()` Methode verwenden, um Strings zu verbinden. Diese Methode nimmt beliebig viele Argumente und verbindet sie zu einem neuen String. Hier ist ein Beispiel, das drei Strings miteinander verbindet:

```TypeScript
let str1 = "Das ";
let str2 = "ist ";
let str3 = "eine Verkettung.";
console.log(str1.concat(str2, str3)); // Ausgabe: Das ist eine Verkettung.
```

## Tiefere Einblicke

Während es einfach erscheinen mag, Strings zu verbinden, gibt es einige wichtige Dinge zu beachten. Beispielsweise können wir keine String-Objekte mit dem `+` Operator verbinden, sondern nur Literal-Strings. Dies bedeutet, dass wir möglicherweise den `toString()` Befehl verwenden müssen, um Objekte in einen String umzuwandeln, bevor wir sie verbinden können.

Wir sollten auch darauf achten, wie viele Strings wir miteinander verbinden. Bei großen Mengen von Strings kann dies zu einer großen Anzahl von Zwischenspeicherungen im Arbeitsspeicher führen, was zu Leistungsproblemen führen kann. In solchen Fällen kann es ratsam sein, stattdessen die `concat()` Methode zu verwenden.

## Siehe auch

- [Offizielle TypeScript Dokumentation zur Verkettung von Strings](https://www.typescriptlang.org/docs/handbook/strings.html#string-literal-types)
- [Tutorial: Das Verbinden von Strings in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-typescript)
- [Weitere Beispiele zur Verkettung von Strings in TypeScript](https://www.w3schools.com/js/js_string_methods.asp)