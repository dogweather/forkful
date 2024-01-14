---
title:                "C#: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Funktion in der Programmierung. Es ermöglicht, die Anzahl der Zeichen in einem Text zu bestimmen und dient als Grundlage für viele weitere Operationen. Es ist also wichtig, die verschiedenen Methoden zur Längenberechnung zu verstehen.

## So geht's

Um die Länge eines Strings in C# zu finden, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der `Length`-Methode. Diese gibt die Anzahl der Zeichen im String zurück.

```C#
string text = "Hallo Welt!";
Console.WriteLine(text.Length);
//Output: 11
```

Eine weitere Möglichkeit ist die Verwendung der `Count()`-Methode, die ebenfalls die Anzahl der Zeichen zurückgibt.

```C#
string text = "Hallo Welt!";
Console.WriteLine(text.Count());
//Output: 11
```

Es ist wichtig zu beachten, dass die Länge eines Strings auch Sonderzeichen und Leerzeichen beinhaltet. Wenn beispielsweise ein Leerzeichen am Anfang oder Ende des Textes steht, wird dies auch als Zeichen gezählt.

## Tiefere Einblicke

Hinter den Kulissen verwendet C# den Datentyp `System.String` für Strings. Dieser Datentyp enthält eine Eigenschaft namens `Length`, die die Länge des Strings speichert. Beim Aufrufen der `Length`-Methode wird diese Eigenschaft abgerufen und die Anzahl der Zeichen zurückgegeben.

Es gibt auch die Möglichkeit, die Länge eines Strings mithilfe einer Schleife selbst zu berechnen. Hierzu können wir die `foreach`-Schleife nutzen und jede Iteration um 1 erhöhen, wie in folgendem Beispiel:

```C#
string text = "Hallo Welt!";
int length = 0;

foreach(char c in text)
{
    length++;
}

Console.WriteLine(length);
//Output: 11
```

Eine weitere interessante Methode ist die Verwendung von `StringInfo`, um auch die Längen von Unicode-Zeichen zu berücksichtigen. Dies ist besonders wichtig, wenn wir mit mehrsprachigen Texten arbeiten.

## Siehe auch

- [Dokumentation zu String.Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [Dokumentation zu String.Count()](https://docs.microsoft.com/en-us/dotnet/api/system.string.count?view=net-5.0)
- [Dokumentation zu StringInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=net-5.0)