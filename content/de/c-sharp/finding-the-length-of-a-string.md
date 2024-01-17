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

## Was & Warum?
Die Länge eines Strings zu finden bedeutet herauszufinden, wie viele Zeichen ein bestimmter Text enthält. Programmierer tun dies, um die Größe von Strings zu bestimmen und sie effektiv zu nutzen.
 
## So geht's:
Es gibt verschiedene Möglichkeiten, die Länge eines Strings in C# zu finden. Eine einfache Methode ist die Verwendung der ```Length``` Eigenschaft, wie im folgenden Beispiel:
 
```C#
string text = "Hallo Welt!";
int length = text.Length;
Console.WriteLine(length); // Output: 11
```
Eine weitere Möglichkeit ist die Verwendung der ```Count``` Funktion, die die Anzahl der Elemente in einem String-Array zurückgibt. Hier ist ein Beispiel, das beide Methoden zeigt:
 
```C#
string text = "Hallo Welt!";
int length1 = text.Length;
int length2 = text.ToCharArray().Count();
Console.WriteLine(length1); // Output: 11
Console.WriteLine(length2); // Output: 11
```

## Tief eintauchen:
Das Finden der Länge eines Strings ist eine grundlegende und häufig verwendete Aufgabe in der Programmierung. Es geht zurück auf die frühen Tage von BASIC, als das Befehlszeilen-Tool ```LEN``` verwendet wurde, um die Länge von Strings zu ermitteln. In C# gibt es mehrere Möglichkeiten, die Länge eines Strings zu finden, einschließlich der Verwendung von Regulären Ausdrücken oder der Implementierung einer eigenen Funktion. Es ist wichtig zu beachten, dass die Länge eines Strings in C# immer die Anzahl der Zeichen und nicht die Anzahl der Bytes im String ist.

## Siehe auch:
- [MSDN-Dokumentation zu String-Länge](https://docs.microsoft.com/de-de/dotnet/api/system.string.length?view=net-5.0)
- [C# - String-Länge Online-Beispiel](https://www.tutorialspoint.com/csharp/csharp_string_length.htm)
- [Video-Tutorial zum Finden der Länge eines Strings in C#](https://www.youtube.com/watch?v=_1BmJlmhPSQ)