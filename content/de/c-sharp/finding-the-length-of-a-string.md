---
title:    "C#: Die Länge eines Strings finden"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Fähigkeit, die jeder C#-Programmierer beherrschen sollte. Es ermöglicht uns, die Größe von Texten zu bestimmen, was in vielen Anwendungsfällen nützlich ist.

## Wie

Das Finden der Länge eines Strings ist in C# sehr einfach. Wir können die Methode `Length` verwenden, die als Eigenschaft für jeden String verfügbar ist. Hier ist ein Beispiel:

```C#
string text = "Hallo, Welt!";
int length = text.Length;
Console.WriteLine("Die Länge des Strings beträgt: " + length);
```
Die Ausgabe dieses Codes wird folgendermaßen aussehen:

`Die Länge des Strings beträgt: 12`

Wie Sie sehen können, gibt es nicht viel zu tun, um die Länge eines Strings zu finden. Wir müssen nur die `Length`-Methode nutzen und das Ergebnis in einer Variable speichern oder direkt ausgeben.

Es ist wichtig zu beachten, dass die `Length`-Methode die Anzahl der Unicode-Zeichen in einem String zählt, nicht die Anzahl der Bytes. Dies kann bei der Verwendung von Sonderzeichen oder nicht-ASCII-Zeichen relevant werden.

## Deep Dive

Wenn wir einen genaueren Blick auf die `Length`-Methode werfen, werden wir feststellen, dass sie tatsächlich ein Attribut ist und keine Methode. Das bedeutet, dass sie keine Parameter annimmt und keine Berechnungen ausführt - sie gibt einfach die Anzahl der Zeichen im String zurück.

Es ist auch erwähnenswert, dass die `Length`-Methode eine der Eigenschaften der Basisklasse `Object` ist, was bedeutet, dass sie für alle C#-Objekte verfügbar ist.

Wir können auch die `Length`-Eigenschaft verwenden, um den Wert eines leeren Strings abzurufen, der in C# durch `""` dargestellt wird. In diesem Fall wird die `Length` auf 0 gesetzt.

## Siehe auch

- [Microsoft Dokumentation über die `Length`-Eigenschaft](https://docs.microsoft.com/de-de/dotnet/api/system.string.length?view=netcore-3.1)
- [C# Strings und Zeichen](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/strings-and-characters/)