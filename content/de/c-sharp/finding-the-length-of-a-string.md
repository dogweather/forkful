---
title:    "C#: Die Länge eines Strings finden"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Warum: Das Finden der Länge einer Zeichenfolge ist eine grundlegende Aufgabe beim Programmieren. Es ermöglicht uns, die Größe eines Textes zu bestimmen und ihn entsprechend zu verarbeiten.

Wie: Die Länge einer Zeichenfolge kann in C# auf verschiedene Arten gefunden werden. Eine Möglichkeit ist die Verwendung der integrierten Methode `Length`, die uns die Anzahl der Zeichen in der Zeichenfolge zurückgibt. Beispielcode:

 ```C#
 string text = "Hallo Welt!";
 int length = text.Length;
 Console.WriteLine(length); // Gibt 11 aus
```

Eine andere Möglichkeit ist die Verwendung der Methode `ToCharArray`, die uns ein Array mit allen Zeichen der Zeichenfolge zurückgibt. Wir können dann einfach die Länge des Arrays bestimmen, um die Länge der Zeichenfolge zu erhalten. Beispielcode:

```C#
 string text = "Hallo Welt!";
 char[] charArray = text.ToCharArray();
 int length = charArray.Length;
 Console.WriteLine(length); // Gibt 11 aus
```

Tipp: Beachten Sie, dass bei der Verwendung von `Length` die Leerzeichen und Sonderzeichen in der Zeichenfolge ebenfalls gezählt werden, während bei der Verwendung von `ToCharArray` diese als separate Zeichen betrachtet werden.

Tiefergehende Informationen: Um die Länge einer Zeichenfolge besser zu verstehen, ist es hilfreich, zu verstehen, wie Zeichen in C# gespeichert werden. In C# werden Zeichen als Unicode-Codepoints gespeichert, was bedeutet, dass nicht jedes Zeichen die gleiche Größe hat und daher die Länge einer Zeichenfolge variieren kann. Zum Beispiel haben einige Sonderzeichen zwei Codepoints, was bedeutet, dass sie als zwei separate Zeichen gezählt werden.

Siehe auch:

- [MSDN-Dokumentation zu `Length`](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netframework-4.8)
- [MSDN-Dokumentation zu `ToCharArray`](https://docs.microsoft.com/en-us/dotnet/api/system.string.tochararray?view=netframework-4.8)
- [Unicode-Codepoints in C#](https://docs.microsoft.com/en-us/dotnet/api/system.char?view=netframework-4.8)