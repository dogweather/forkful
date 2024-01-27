---
title:                "Überprüfen, ob ein Verzeichnis existiert"
date:                  2024-01-19
html_title:           "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Überprüfen, ob ein Verzeichnis existiert, bedeutet einfach, zu kontrollieren, ob ein bestimmter Ordnerpfad auf dem Dateisystem vorhanden ist. Programmierer machen das, um Fehler zu vermeiden, die auftreten können, wenn sie versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen oder Daten darin zu speichern.

## Wie macht man das:
Hier ist ein code-Schnipsel, der zeigt, wie man in C# überprüft, ob ein Verzeichnis existiert.

```C#
using System;
using System.IO;

class DirectoryChecker
{
    static void Main()
    {
        string dirPath = @"C:\BeispielVerzeichnis";

        if (Directory.Exists(dirPath))
        {
            Console.WriteLine("Das Verzeichnis existiert!");
        }
        else
        {
            Console.WriteLine("Das Verzeichnis existiert nicht.");
        }
    }
}
```
Sample Output:
```
Das Verzeichnis existiert!
```
oder
```
Das Verzeichnis existiert nicht.
```

## Tiefgang
Es ist wichtig, weil Dateioperationen ohne vorherige Überprüfung der Pfadgültigkeit zu unvorhersehbaren Ergebnissen und Laufzeitfehlern führen können. Historisch gesehen, hat seit den frühen Versionen von .NET das `System.IO`-Namespace Funktionen für derartige Überprüfungen geboten. Alternativen zur `Directory.Exists`-Methode sind unter anderem das Abfangen von Ausnahmen, die beim Versuch des Zugriffs auf ein nicht vorhandenes Verzeichnis geworfen werden. Jedoch ist das Abfragen mit `Exists` präziser und weniger aufwendig als die Behandlung von Ausnahmen.

## Siehe Auch
- Microsoft Docs zur `Directory.Exists`-Methode: https://docs.microsoft.com/de-de/dotnet/api/system.io.directory.exists
- Microsoft Docs für Datei- und Stream-E/A: https://docs.microsoft.com/de-de/dotnet/standard/io
- Zu Behandlung von Pfad- und Dateisystem-Fehlern: https://docs.microsoft.com/de-de/dotnet/standard/io/handling-io-errors
