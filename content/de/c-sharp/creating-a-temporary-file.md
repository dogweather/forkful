---
title:    "C#: Erstellen einer temporären Datei"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Beim Programmieren in C# gibt es viele Gründe, warum man temporäre Dateien erstellen würde. Hier sind einige Beispiele:

- Um Zwischenergebnisse zu speichern und später weiterzuverwenden.
- Wenn man mit großen Dateien arbeitet und diese nicht komplett im Arbeitsspeicher halten möchte.
- Um temporäre Konfigurationsdateien oder Logs zu erstellen.
- In Fällen, in denen der Zugriff auf eine Datei kurzfristig benötigt wird und sie danach nicht mehr gebraucht wird.

## Wie man eine temporäre Datei in C# erstellt

Um eine temporäre Datei in C# zu erstellen, können wir die Klasse `Path` aus dem `System.IO` Namespace verwenden. Der folgende Code zeigt, wie man eine temporäre Textdatei erstellt und Text in diese Datei schreibt:

```C#
string tempFilePath = Path.GetTempFileName(); // Erstellt eine temporäre Datei
using (StreamWriter writer = new StreamWriter(tempFilePath)) // Öffnet die Datei zum Schreiben
{
    writer.WriteLine("Hello World!"); // Schreibt Text in die Datei
}
Console.WriteLine($"Temporäre Datei erstellt unter {tempFilePath}"); // Gibt den Pfad der Datei aus
```

Die Ausgabe des obigen Codes würde wie folgt aussehen:

```
Temporäre Datei erstellt unter C:\Users\username\AppData\Local\Temp\tmp7A26.tmp
```

Um die erstellte Datei zu löschen, können wir die `File` Klasse aus demselben Namespace verwenden:

```C#
File.Delete(tempFilePath); // Löscht die temporäre Datei
```

## Tiefer Einblick

Wenn wir uns den oben gezeigten Code genauer ansehen, können wir sehen, dass die `GetTempFileName()` Methode eine eindeutige temporäre Datei erstellt und den vollständigen Pfad der Datei zurückgibt. Diese Datei wird automatisch im Ordner "%temp%" erstellt, der auf jedem Betriebssystem unterschiedlich sein kann.

Außerdem ist es wichtig zu beachten, dass die erstellte temporäre Datei automatisch gelöscht wird, sobald das Programm beendet wird. Wenn die Datei länger als die Lebensdauer des Programms benötigt wird, sollten wir die `Path.GetRandomFileName()` Methode verwenden, um eine eindeutige Dateiname-Base zu erstellen, und dann die `File.Create()` Methode verwenden, um die Datei zu erstellen.

## Siehe auch

- [MSDN - Path Class (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.io.path?view=net-5.0)
- [MSDN - File Class (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.io.file?view=net-5.0)