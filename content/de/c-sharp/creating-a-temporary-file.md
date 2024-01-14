---
title:                "C#: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Es gibt verschiedene Gründe, warum man beim Programmieren vorübergehende Dateien erstellen möchte. Eine mögliche Anwendung ist zum Beispiel das Speichern von Zwischenergebnissen während der Ausführung eines Programms.

# Wie erstellt man eine temporäre Datei in C#

Um eine temporäre Datei in C# zu erstellen, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der Klasse "Path" aus dem Namespace "System.IO". Hier ein Beispielcode:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine("Temporäre Datei wurde erstellt unter: " + tempFilePath);
```

Die Methode "GetTempFileName" erstellt automatisch eine temporäre Datei im Standard- temporären Ordner des Systems. Das Ergebnis wird als String zurückgegeben und kann verwendet werden, um auf die erstellte Datei zuzugreifen.

Eine andere Möglichkeit ist die Verwendung der Klasse "FileStream" aus dem Namespace "System.IO". Hier ein Beispielcode:

```C#
string tempFilePath = Path.GetTempFileName();
FileStream tempFile = File.Create(tempFilePath);
Console.WriteLine("Temporäre Datei wurde erstellt unter: " + tempFilePath);
```

In diesem Beispiel wird mit der Methode "Create" der Klasse "File" ein FileStream-Objekt erstellt. Dieses Objekt kann dann verwendet werden, um Daten in die temporäre Datei zu schreiben.

# Tiefergehende Informationen

Bei der Verwendung von temporären Dateien sollte man darauf achten, sie nach der Verwendung wieder zu löschen, um den Speicher des Systems nicht unnötig zu belasten. Dafür kann die Methode "Delete" der Klasse "File" verwendet werden.

Außerdem ist es wichtig, die Zugriffsrechte für die temporäre Datei zu beachten. In manchen Fällen ist es nötig, die Datei für andere Prozesse les- und schreibbar zu machen. Hierzu kann die Methode "SetAccessControl" der Klasse "File" verwendet werden.

# Siehe auch

- [Microsoft Dokumentation zu temporären Dateien](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.createtemporary?view=net-5.0)
- [C# Tutorial zu Datei- und Verzeichnisoperationen](https://www.c-sharpcorner.com/blogs/file-and-directory-operation-using-c-sharp1)