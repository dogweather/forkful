---
title:                "Die Länge eines Strings finden"
html_title:           "Javascript: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge einer Zeichenkette zu kennen ist wichtig, um effektiv mit Texten in Javascript zu arbeiten. Es kann beispielsweise hilfreich sein, um die Anzahl der Buchstaben oder Wörter in einem Text zu zählen oder um sicherzustellen, dass die Eingabe eines Nutzers nicht zu lang ist.

## Wie geht's

Um die Länge einer Zeichenkette in Javascript zu finden, können Sie die `.length` Eigenschaft verwenden. Schauen wir uns ein Beispiel an:

```Javascript
let string = "Hallo Welt!";
console.log(string.length); // Output: 11
```

Hier haben wir die Variable `string` mit dem Wert "Hallo Welt!" erstellt und dann die `.length` Eigenschaft aufgerufen, um die Länge der Zeichenkette zu finden. Wir können sehen, dass das Ergebnis 11 ist, da die Leerzeichen und Satzzeichen ebenfalls zur Länge der Zeichenkette zählen.

Es ist auch möglich, die `.length` Eigenschaft auf Zeichenketten zu verwenden, die aus Variablen bestehen. Schauen wir uns ein weiteres Beispiel an:

```Javascript
let name = "Max";
let greeting = `Hallo ${name}!`;
console.log(greeting.length); // Output: 8
```

Hier haben wir die Variable `greeting` erstellt, die den Wert "Hallo Max!" hat, indem wir den Variablen `name` in einer Template Literal Zeichenkette verwenden. Auch hier zählen die Leerzeichen und Satzzeichen zur Länge der Zeichenkette.

Es ist wichtig zu beachten, dass die `.length` Eigenschaft nur auf Zeichenketten verwendet werden kann, daher funktioniert sie nicht auf Zahlen oder booleschen Werten.

## Tiefer eintauchen

Die `.length` Eigenschaft gibt uns die Gesamtzahl der Zeichen in einer Zeichenkette zurück, einschließlich Leerzeichen und Satzzeichen. Wenn wir jedoch nur die Anzahl der Buchstaben in einer Zeichenkette finden möchten, können wir die `.replace()` Methode verwenden, um alle Leerzeichen und Satzzeichen durch leere Zeichen zu ersetzen und dann die Länge der resultierenden Zeichenkette zu finden.

Schauen wir uns ein Beispiel an:

```Javascript
let string = "Hallo Welt!";
let newString = string.replace(/[^\w]/g, ""); // Entfernt alle Leerzeichen und Satzzeichen
console.log(newString.length); // Output: 9
```

Hier haben wir die `.replace()` Methode verwendet, um alle Leerzeichen und Satzzeichen aus der Zeichenkette "Hallo Welt!" zu entfernen und in der Variablen `newString` zu speichern. Dann können wir die Länge dieser Variablen abrufen, um die Anzahl der Buchstaben zu finden.

## Siehe auch

- [MDN Web Docs: String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN Web Docs: String replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)