---
title:    "TypeScript: Die Länge eines Strings finden"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist ein grundlegender Bestandteil der Programmierung in TypeScript. Es ermöglicht es uns, die Anzahl der Zeichen in einer Zeichenkette zu bestimmen und somit bestimmte Funktionalitäten in unseren Programmen zu implementieren.

## Wie geht es

Um die Länge einer Zeichenkette in TypeScript zu finden, können wir die integrierte Methode `length` verwenden. Diese Methode gibt die Anzahl der Zeichen in der Zeichenkette zurück. Hier ist ein Beispielcode:

```TypeScript
let name: string = "Max Mustermann";
let length: number = name.length;
console.log(length);
```

Der obige Code wird die Länge der Zeichenkette "Max Mustermann" ausgeben, die 14 Zeichen beinhaltet. Wir können auch die `length` Methode direkt auf eine Zeichenkette anwenden, ohne sie einer Variablen zuzuweisen.

```TypeScript
let length: number = "Hallo Welt".length;
console.log(length);
```

Diesmal wird die Ausgabe 10 sein, da die Zeichenkette "Hallo Welt" aus 10 Zeichen besteht.

## Deep Dive

Es gibt mehr zu beachten, als nur die `length` Methode zu verwenden, um die Länge einer Zeichenkette zu finden. Zum Beispiel werden Unicode-Zeichen in TypeScript als einzelne Zeichen behandelt, auch wenn sie aus mehreren Codepunkten bestehen. Daher kann es sein, dass die Länge einer Zeichenkette nicht immer der Anzahl der sichtbaren Zeichen entspricht.

Darüber hinaus können wir auch die `slice` Methode verwenden, um einen Teil einer Zeichenkette zu extrahieren und somit die Länge zu manipulieren. Hier ist ein Beispiel:

```TypeScript
let name: string = "Anna Müller";
let length: number = name.length;
console.log(length);
let shortName: string = name.slice(0, 4);
length = shortName.length;
console.log(length);
```

Die Ausgabe wird zuerst 11 sein, da die Zeichenkette "Anna Müller" 11 Zeichen hat. Aber nachdem wir mit der `slice` Methode einen Teil der Zeichenkette extrahiert haben, ist die Ausgabe jetzt 4.

## Siehe auch

- [Anleitung zu den TypeScript-Grundlagen](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [Offizielle TypeScript-Referenz zur String-Methode](https://www.typescriptlang.org/docs/handbook/reference/de-DE/string.html)