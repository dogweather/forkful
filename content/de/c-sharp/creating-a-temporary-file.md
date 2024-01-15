---
title:                "Erstellen einer temporären Datei"
html_title:           "C#: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
In der Welt der Programmierung gibt es viele Anwendungen und Szenarien, in denen das Erstellen einer temporären Datei notwendig ist. Es kann beispielsweise verwendet werden, um Zwischenergebnisse zu speichern, Daten zu puffern oder vorübergehende Dateien für einen bestimmten Prozess zu erstellen. Das Erstellen von temporären Dateien kann auch hilfreich sein, um Systemressourcen effizienter zu nutzen.

## Wie geht das?
Um eine temporäre Datei in C# zu erstellen, gibt es verschiedene Ansätze. Hier sind zwei Beispiele, die jeweils unterschiedliche Methoden verwenden.

### Beispiel 1: Verwenden von System.IO.Path.GetTempFileName
```C#
string tempFile = Path.GetTempFileName();
Console.WriteLine("Temporäre Datei erstellt unter: " + tempFile);
```

### Beispiel 2: Manuell erstellte temporäre Datei
```C#
string tempPath = Path.GetTempPath();
string randomFileName = Path.GetRandomFileName();
string tempFilePath = Path.Combine(tempPath, randomFileName);
using (FileStream fs = File.Create(tempFilePath))
{
    Console.WriteLine("Temporäre Datei manuell erstellt unter: " + tempFilePath);
}
```

Die Ausgabe für beide Beispiele könnte wie folgt aussehen:
```
Temporäre Datei erstellt unter: C:\Users\Benutzer\AppData\Local\Temp\tmpB4A8.tmp
Temporäre Datei manuell erstellt unter: C:\Users\Benutzer\AppData\Local\Temp\tmp672A.tmp
```

## Tieferer Einblick
Das Erstellen einer temporären Datei kann klein und einfach erscheinen, aber es gibt einige wichtige Überlegungen, die berücksichtigt werden müssen. Zum Beispiel ist es wichtig, dass die erstellte temporäre Datei eindeutig und nicht von anderen Prozessen verwendet wird. Dies bedeutet, dass ein zufälliger Dateiname und ein sicherer Speicherort gewählt werden sollten. Außerdem ist es wichtig, die temporäre Datei nach ihrer Verwendung zu löschen, um Speicherplatz zu sparen und das System sauber zu halten. In C# kann dies einfach durch Verwendung der `File.Delete()` Methode erreicht werden.

## Siehe auch
- [Microsoft Docs - Creating Temporary Files in .NET](https://docs.microsoft.com/en-us/dotnet/standard/io/creating-temporary-files) 
- [C# Corner - Working with Temporary Files in C#](https://www.c-sharpcorner.com/article/working-with-temporary-files-in-c-sharp/) 
- [CodeProject - Creating Temporary Files in C#](https://www.codeproject.com/Articles/7336/Tips-tricks-Creating-Temporary-Files-in-C)