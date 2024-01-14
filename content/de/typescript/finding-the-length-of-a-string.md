---
title:    "TypeScript: Ermitteln der Länge eines Strings"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Ermittlen der Länge eines Strings ist eine häufige Aufgabe in der Programmierung. Es ist wichtig, die Länge eines Strings zu kennen, um beispielsweise Strings korrekt zu validieren oder sie in einer längenbeschränkten Datenbank einzufügen. In diesem Blog-Beitrag werden wir uns ansehen, wie man die Länge eines Strings in TypeScript ermitteln kann.

## Wie

Um die Länge eines Strings zu finden, gibt es in TypeScript verschiedene Möglichkeiten. Die einfachste und am häufigsten genutzte Methode ist die Verwendung der `length` Eigenschaft eines Strings, die die Gesamtzahl der Zeichen im String zurückgibt.

```TypeScript
const text: string = "Hallo Welt";
console.log(text.length); // Output: 10
```

Alternativ kann auch die `size` Methode des `string` Objekts verwendet werden, die ebenfalls die Länge des Strings zurückgibt.

```TypeScript
const text: string = "Hallo Welt";
console.log(text.size); // Output: 10
```

Es ist auch möglich, die `length` Eigenschaft von `Array` zu verwenden, um die Länge eines Strings zu ermitteln. Da Strings in TypeScript als Arrays von Zeichen behandelt werden, kann die `length` Eigenschaft auf sie angewendet werden.

```TypeScript
const text: string = "Hallo Welt";
console.log(text.split("").length); // Output: 10
```

## Deep Dive

Die `length` Eigenschaft ist nicht nur auf Strings beschränkt, sondern kann auch auf anderen Datentypen wie Arrays, Maps und Sets angewendet werden. Es ist wichtig zu beachten, dass die `length` Eigenschaft die tatsächliche Anzahl der Elemente im Datentyp zurückgibt, nicht die Anzahl der indizierten Elemente.

Ein weiterer wichtiger Aspekt ist, dass die `length` Eigenschaft nicht die tatsächliche Länge eines Strings in Bezug auf Bytes oder Speicherplatz zurückgibt. Sie gibt nur die Anzahl der Zeichen im String zurück.

Wir können auch die `toString()` Methode verwenden, um einen Datentyp in einen String umzuwandeln und dann die `length` Eigenschaft darauf anzuwenden.

```TypeScript
// Anzahl der Zeichen im Integer-Wert 12345
const number: number = 12345;
console.log(number.toString().length); // Output: 5
```

## Siehe auch

- [Offizielle TypeScript Dokumentation zur Länge eines Strings](https://www.typescriptlang.org/docs/handbook/strings.html#length)
- [Stack Overflow Beitrag zum Ermitteln der Länge eines Strings in TypeScript](https://stackoverflow.com/questions/32714563/typescript-get-the-length-of-a-string)
- [Tutorial zur Arbeit mit Strings in TypeScript](https://blog.logrocket.com/working-strings-typescript/)