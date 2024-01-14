---
title:                "C#: Erstellen einer temporären Datei"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Temporary-Dateien sind in der Programmierung unverzichtbar, wenn es darum geht, Daten vorübergehend zu speichern, um sie später wieder zu verwenden oder zu verarbeiten. Sie können hilfreich sein, besonders in Situationen, in denen der Computer nicht genügend Speicherplatz hat, um alle Daten in den Arbeitsspeicher zu laden.

## So erstellen Sie eine Temporary-Datei

Es gibt verschiedene Möglichkeiten, in C# eine Temporary-Datei zu erstellen. Eine davon ist die Verwendung der Klasse "Path" aus dem Namespace System.IO. Mit der Methode "GetTempFileName" können Sie eine eindeutige Temporary-Datei erstellen und einen bestimmten Dateipfad zurückgeben. Ein Beispiel für die Verwendung dieser Methode könnte wie folgt aussehen:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine($"Temporary file created at: {tempFilePath}");
```

Die Ausgabe dieses Codes würde die folgende Zeile ausgeben: "Temporary file created at: C:\Users\Username\AppData\Local\Temp\6ndm3qka.tmp". Beachten Sie, dass der Dateipfad je nach Betriebssystem und Benutzer unterschiedlich sein kann.

## Tiefere Einblicke

Beim Erstellen von Temporary-Dateien müssen Sie sich möglicherweise auch Gedanken über die Sicherheit machen. Da Temporary-Dateien in der Regel unverschlüsselt und ungeschützt auf dem Computer gespeichert werden, könnten sie von anderen Programmen oder von bösartiger Software eingesehen oder verändert werden. Aus diesem Grund ist es wichtig, sicherzustellen, dass nur berechtigte Personen oder Programme auf die Temporary-Dateien zugreifen können.

Eine Möglichkeit, die Sicherheit von Temporary-Dateien zu erhöhen, ist die Verwendung von speziellen Berechtigungen über die Klasse "File" des Namespaces System.IO. Mit der Methode "SetAccessControl" können Sie die Sicherheitsberechtigungen für die Temporary-Datei festlegen. Ein Codebeispiel könnte wie folgt aussehen:

```C#
FileSecurity tempFileSecurity = File.GetAccessControl(tempFilePath);
tempFileSecurity.AddAccessRule(new FileSystemAccessRule("Users", FileSystemRights.ReadAndExecute, AccessControlType.Allow));
File.SetAccessControl(tempFilePath, tempFileSecurity);
```

In diesem Beispiel wird der Datei "Users" das Recht eingeräumt, auf die Temporary-Datei zuzugreifen und diese auszuführen.

## Siehe auch

- [Microsoft Docs: Path Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [Microsoft Docs: FileSecurity Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.security.accesscontrol.filesecurity?view=netcore-3.1)