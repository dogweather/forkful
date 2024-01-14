---
title:    "C#: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Aspekt in der C# Programmierung. Es kann hilfreich sein, um sicherzustellen, dass ein bestimmtes Verzeichnis vorhanden ist, bevor man versucht, damit zu interagieren oder um zu vermeiden, dass unnötiger Code ausgeführt wird.

## Wie funktioniert es?

Um zu überprüfen, ob ein Verzeichnis existiert, kann die `Directory.Exists()`-Methode verwendet werden. Diese Methode gibt einen booleschen Wert zurück, der angibt, ob das angegebene Verzeichnis vorhanden ist oder nicht.

```C#
string path = @"C:\Users\Benutzer\Desktop\Beispielverzeichnis";

if (Directory.Exists(path))
{
    Console.WriteLine("Das Verzeichnis existiert.");
}
else
{
    Console.WriteLine("Das Verzeichnis existiert nicht.");
}
```

Im obigen Beispiel wird überprüft, ob das Verzeichnis "Beispielverzeichnis" im angegebenen Pfad vorhanden ist. Wenn dies der Fall ist, wird die entsprechende Nachricht auf der Konsole ausgegeben. Wenn nicht, wird die andere Nachricht ausgegeben.

## Tiefgehende Informationen

Bei der Verwendung der `Directory.Exists()`-Methode ist es wichtig zu beachten, dass sie nur prüft, ob das Verzeichnis zum Zeitpunkt der Ausführung des Codes vorhanden ist. Es kann jedoch vorkommen, dass das Verzeichnis zu einem späteren Zeitpunkt gelöscht wird oder nicht mehr zugänglich ist. Daher kann es ratsam sein, zusätzliche Validierungen einzubauen, um sicherzustellen, dass das Verzeichnis weiterhin existiert, bevor darauf zugegriffen wird.

Zusätzlich kann die `Directory.Exists()`-Methode auch verwendet werden, um zu überprüfen, ob ein Verzeichnis vorhanden ist, bevor man versucht, es zu erstellen. Wenn das Verzeichnis bereits existiert, wird die Erstellung übersprungen und es wird keine Fehlermeldung ausgegeben.

## Siehe auch

- [Directory.Exists() - Microsoft Documentation](https://docs.microsoft.com/de-de/dotnet/api/system.io.directory.exists)
- [Validieren von Datei- und Verzeichnispfaden - Microsoft Documentation](https://docs.microsoft.com/de-de/dotnet/standard/io/file-path-operations#validating-file-and-directory-paths)