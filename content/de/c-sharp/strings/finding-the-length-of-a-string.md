---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:47:03.985386-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu bestimmen bedeutet, die Anzahl der Zeichen zu zählen, die er enthält. Das ist nützlich, um Text zu verarbeiten, Grenzen zu überprüfen oder Daten validieren zu können.

## Anleitung:
Um die Länge eines Strings in C# zu bekommen, verwenden wir die `Length`-Eigenschaft. Sieh dir das Beispiel an:

```C#
string beispiel = "Hallo Welt";
Console.WriteLine(beispiel.Length);  // Gibt die Länge aus: 10
```

Ausgabe:

```
10
```

## Tiefergehende Einblicke:
Die `.Length`-Eigenschaft gibt es in C# seit der ersten Version. Sie gibt einen `int` zurück, der die Anzahl der `char`-Elemente im String anzeigt. Da `.Length` eine Eigenschaft ist, erfolgt der Zugriff schnell und effizient, ohne dass der String durchlaufen werden muss. Alternative Methoden, wie etwa die Verwendung eines For-Loops, um manuell die Zeichen zu zählen, sind unnötig und ineffizient. In der Unicode-Welt kann die Länge irreführend sein, da einige Zeichen als mehrere `char`-Instanzen repräsentiert werden – das aber nur als Randnote.

## Siehe Auch:
- Microsoft-Dokumentation zur `Length`-Eigenschaft: [Microsoft Docs: String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- Ein Artikel zur Unicode-Verarbeitung in C#: [Understanding Characters, Strings and Encoding in .NET](https://www.codeproject.com/Articles/17201/Understanding-Characters-Strings-and-Encoding-in-N)
- Stack Overflow Diskussionen zur String-Längenbestimmung: [Stack Overflow: How to get the length of a string](https://stackoverflow.com/questions/228038/best-way-to-reverse-a-string)
