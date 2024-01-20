---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "C#: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein Vorgang, bei dem Code geschrieben wird, um zu überprüfen, ob ein bestimmtes Verzeichnis auf dem Computer oder Server existiert. Programmierer tun dies, um Fehler zu vermeiden, die auftreten können, wenn sie versuchen, auf ein nicht vorhandenes Verzeichnis zuzugreifen.

## Wie?

Der folgende Code zeigt, wie man in C# überprüft, ob ein Verzeichnis existiert:

```C#
using System.IO;

string dirPath = @"C:\Beispielverzeichnis";

if (Directory.Exists(dirPath))
{
    Console.WriteLine("Verzeichnis existiert.");
}
else
{
    Console.WriteLine("Verzeichnis existiert nicht.");
}
```
Wenn das Verzeichnis existiert, wird die Ausgabe "Verzeichnis existiert." sein. Ansonsten ist die Ausgabe "Verzeichnis existiert nicht."

## Vertiefung

Historisch gesehen mussten Programmierer über Betriebssystembefehle überprüfen, ob ein Verzeichnis existiert - ein mühsamer und fehleranfälliger Prozess. Glücklicherweise bietet C# nun die eingebaute Methode `Directory.Exists()`, um diese Aufgabe zu erleichtern.

Alternativ könnten Programmierer Exceptions-Handling verwenden und den Verzeichnispfad direkt öffnen. Wenn ein `DirectoryNotFoundException` geworfen wird, bedeutet dies, dass das Verzeichnis nicht existiert. Diese Methode tendiert dazu, langsamer zu sein, da das Werfen und Fangen von Ausnahmen im Vergleich zum einfachen Abfragen des Verzeichnispfades aufwendiger ist.

Die `Directory.Exists()` Methode funktioniert, indem sie einen API-Aufruf macht, um das Dateisystem zu fragen, ob das Verzeichnis existiert. Dies ist eine sehr effiziente Methode, da sie direkt mit dem Betriebssystem spricht.

## Siehe Auch

- [DirectoryInfo.Exists Eigenschaft](https://docs.microsoft.com/de-de/dotnet/api/system.io.directoryinfo.exists?view=net-5.0)
- [Datei- und Verzeichnisnamen in .NET](https://docs.microsoft.com/de-de/dotnet/standard/io/file-path-formats)