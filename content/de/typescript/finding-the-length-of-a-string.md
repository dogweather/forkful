---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "TypeScript: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Man findet oft die Länge eines Strings, um zu überprüfen, ob er die gewünschte Anzahl von Zeichen hat oder um ihn in verschiedene Teile zu unterteilen. In diesem Artikel erfahren Sie, wie Sie mithilfe von TypeScript die Länge eines Strings ermitteln können.

## How To

Es gibt verschiedene Möglichkeiten, die Länge eines Strings in TypeScript zu finden. Hier sind einige Beispiele:

```typescript
// erstellen eines Strings
let message: string = "Hallo Welt";

// Verwendung von length Eigenschaft
let lengthOfString: number = message.length;
console.log(lengthOfString); // Ausgabe: 10

// Verwendung der split() Methode
let parts: string[] = message.split('');
console.log(parts.length); // Ausgabe: 10
```

In diesem Beispiel haben wir zuerst einen String `message` erstellt und dann die `length` Eigenschaft verwendet, um die Länge des Strings zu erhalten. Es gibt auch die `split()` Methode, mit der Sie den String in einzelne Teile aufteilen und dann die Länge des neuen Arrays bestimmen können. Beide Methoden ergeben dasselbe Ergebnis.

## Deep Dive

Die `length` Eigenschaft gibt die Anzahl der Zeichen eines Strings zurück. Es ist wichtig zu beachten, dass auch Leerzeichen und Sonderzeichen zur Länge des Strings zählen. Zum Beispiel hat der String "Hello World!" eine Länge von 12, da auch das Leerzeichen und das Ausrufezeichen mitgezählt werden.

Wenn Sie statt der Anzahl der Zeichen die Anzahl der Wörter in einem String ermitteln möchten, können Sie die `split()` Methode verwenden und dann die Länge des resultierenden Arrays bestimmen. Beachten Sie jedoch, dass auch hier Leerzeichen und Satzzeichen als separate Elemente gezählt werden.

## Siehe auch

- [TypeScript Dokumentation](https://www.typescriptlang.org/docs/)
- [String.length() auf MDN](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [String.split() auf MDN](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/split)