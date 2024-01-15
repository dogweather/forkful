---
title:                "Die Länge eines Strings finden"
html_title:           "C#: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Ermitteln der Länge einer Zeichenfolge ist eine grundlegende und häufig verwendete Funktion in der Programmierung. Sie ermöglicht es uns, die genaue Größe einer Zeichenfolge zu kennen und diese Informationen in unserem Code verwenden zu können. 

## Wie geht's

Um die Länge einer Zeichenfolge in C# zu finden, können wir die Methode ".Length" verwenden. Zum Beispiel: 

```C#
string text = "Hallo Welt";
int length = text.Length;
Console.WriteLine("Die Länge der Zeichenfolge beträgt: " + length);
```

Dieser Code wird "Die Länge der Zeichenfolge beträgt: 11" ausgeben, da es insgesamt 11 Zeichen in der Zeichenfolge "Hallo Welt" gibt. Wenn die Zeichenfolge jedoch leer ist, wird der Wert 0 zurückgegeben.

Es ist wichtig zu beachten, dass die Methode ".Length" nur auf Zeichenfolgen oder Arrays verwendet werden kann und nicht auf anderen Datentypen.

## Tiefergehende Information

In C# wird die Länge einer Zeichenfolge durch die Anzahl der Unicode-Zeichen in der Zeichenfolge bestimmt. In der veranschaulichten Beispielimplementierung der ".Length"-Methode wird dies durch die Eigenschaft "länge" des "CodePageDataLayer"-Objekts ausgeführt. Die Unicode-Zeichen werden in UTF-16-Codierung gespeichert, was bedeutet, dass die Länge einer Zeichenfolge in C# immer ein Vielfaches von 2 sein wird.

Ein weiterer wichtiger Punkt ist, dass die Methode ".Length" eine "read-only"-Eigenschaft ist, was bedeutet, dass sie nicht geändert werden kann. Wir können also die Länge einer Zeichenfolge nicht direkt modifizieren, sondern nur abfragen.

## Siehe auch

- [String.Length Eigenschaft (C#-Referenz)](https://docs.microsoft.com/de-de/dotnet/api/system.string.length?view=netcore-3.1)
- [UTF-16 (Wikipedia)](https://de.wikipedia.org/wiki/UTF-16)
- [Unicode (Wikipedia)](https://de.wikipedia.org/wiki/Unicode)