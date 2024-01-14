---
title:                "C#: Die Länge eines Strings finden"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge einer Zeichenkette oder eines Strings zu finden, ist eine grundlegende Aufgabe in der Programmierung. Es ist wichtig zu wissen, wie viele Zeichen ein String enthält, um korrekt damit zu arbeiten und Fehler zu vermeiden.

## Wie man es macht

Die Länge eines Strings in C# kann mithilfe der Methode `Length` in der Klasse `String` ermittelt werden. Hier ist ein Beispielcode, der demonstriert, wie man dies in C# macht:

```C#
string text = "Hallo Welt";
int length = text.Length;
Console.WriteLine(length); // Output: 11
```

Der Code definiert eine Variable `text` und weist ihr den Wert "Hallo Welt" zu. Dann wird die Methode `Length` aufgerufen, um die Länge des Strings zu ermitteln und in der Variablen `length` zu speichern. Schließlich wird die Länge ausgegeben.

Ein weiteres Beispiel zeigt, wie man die Länge eines Strings in einem Array von Strings ermitteln kann:

```C#
string[] words = {"Hallo", "Welt", "wie", "geht", "es?"};
Console.WriteLine(words[4].Length); // Output: 3
```

Hier wird die Länge des fünften Elements in dem Array `words` ausgegeben.

## Tiefer eintauchen

Um die Länge eines Strings in C# zu finden, verwendet die `Length`-Methode die Eigenschaft `Length` des Typs `int`. Diese Eigenschaft gibt die Anzahl der Elemente in einem String oder einem Array zurück. Wenn es sich bei dem Objekt um ein String handelt, zählt die Eigenschaft die Anzahl der Unicode-Zeichen in dem String. Wenn es sich um ein Array von Strings handelt, zählt die Eigenschaft die Anzahl der Elemente in dem Array.

Die `Length`-Methode ist eine der nützlichsten Methoden in der Klasse `String` und wird häufig verwendet, um die Länge eines Strings zu überprüfen, bevor man damit arbeitet.

## Siehe auch

- [C# String Class](https://docs.microsoft.com/de-de/dotnet/api/system.string?view=netcore-3.1)
- [C# Arrays](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/arrays/)
- [C# Length Property](https://docs.microsoft.com/de-de/dotnet/api/system.array.length?view=netcore-3.1)