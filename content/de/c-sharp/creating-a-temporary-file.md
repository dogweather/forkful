---
title:    "C#: Eine temporäre Datei erstellen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine wichtige Technik in der C# Programmierung, die es ermöglicht, temporäre Daten zu speichern, die nur für einen bestimmten Prozess oder eine bestimmte Sitzung benötigt werden. Dies kann bei der Verarbeitung von großen Datensätzen oder beim Testen von Code hilfreich sein.

## Wie erstellt man eine temporäre Datei in C#

Um eine temporäre Datei in C# zu erstellen, kann man die "Path.GetTempFileName()" Methode verwenden. Diese Methode erstellt automatisch eine temporäre Datei mit einem zufälligen Namen am angegebenen Pfad. Hier ist ein Beispiel:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine(tempFilePath); // Ausgabe: C:\Users\User\AppData\Local\Temp\tmp1234.tmp
```

## Tiefgehende Informationen

Die erstellte temporäre Datei wird standardmäßig im temporären Ordner des aktuellen Benutzers gespeichert. Dieser Ordner kann je nach Betriebssystem unterschiedlich sein. Man kann jedoch auch einen bestimmten Pfad angeben, indem man die "Path.Combine()" Methode verwendet. Es ist auch wichtig zu beachten, dass die erstellte Datei automatisch gelöscht wird, sobald das Programm beendet wird oder der Benutzer sich abmeldet.

## Siehe auch

- [Microsoft Dokumentation zu Path.GetTempFileName()](https://docs.microsoft.com/de-de/dotnet/api/system.io.path.gettempfilename)
- [Tutorial: Temporäre Dateien in C# erstellen](https://www.tutorialspoint.com/how-to-create-temporary-files-in-c-sharp)
- [Stack Overflow Diskussion über das Löschen von temporären Dateien in C#](https://stackoverflow.com/questions/4339397/delete-tmp-files-in-c-sharp)