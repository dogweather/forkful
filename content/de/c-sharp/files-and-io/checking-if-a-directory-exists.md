---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases: - /de/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:07.187714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis in C# existiert, involviert die Verifizierung der Präsenz eines Ordners in einem spezifizierten Pfad im Dateisystem. Programmierer tun dies, um Fehler zu vermeiden, wie zum Beispiel den Versucht, aus einem nicht existierenden Verzeichnis zu lesen oder in dieses zu schreiben, was eine flüssigere Manipulation von Dateien und Verzeichnissen gewährleistet.

## Wie zu:

### Unter Verwendung von System.IO

C# stellt den `System.IO` Namespace bereit, der die `Directory` Klasse enthält und somit eine direkte Möglichkeit bietet, die Existenz eines Verzeichnisses über die `Exists` Methode zu prüfen.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Prüfen, ob das Verzeichnis existiert
        bool directoryExists = Directory.Exists(directoryPath);

        // Das Ergebnis ausgeben
        Console.WriteLine("Verzeichnis existiert: " + directoryExists);
    }
}
```

**Beispielausgabe:**

```
Verzeichnis existiert: False
```

Falls das Verzeichnis unter dem Pfad `C:\ExampleDirectory` tatsächlich existiert, würde die Ausgabe `True` sein.

### Verwendung von System.IO.Abstractions für Unit-Tests

Wenn es darum geht, Ihren Code unit-testbar zu machen, besonders wenn er mit dem Dateisystem interagiert, ist das `System.IO.Abstractions` Paket eine beliebte Wahl. Es ermöglicht Ihnen, Dateisystemoperationen in Ihren Tests zu abstrahieren und zu mocken. Hier sehen Sie, wie Sie auf diese Weise die Existenz eines Verzeichnisses überprüfen können:

Zuerst sollten Sie sicherstellen, dass Sie das Paket installiert haben:

```
Install-Package System.IO.Abstractions
```

Danach können Sie ein `IFileSystem` in Ihre Klasse injizieren und es verwenden, um zu prüfen, ob ein Verzeichnis existiert, was das Unit-Testing erleichtert.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Verzeichnis existiert: " + directoryExists);
    }
}
```

**Beispielausgabe:**

```
Verzeichnis existiert: False
```

Dieser Ansatz entkoppelt Ihre Anwendungslogik vom direkten Dateisystemzugriff und macht Ihren Code modularer, testbarer und wartbarer.
