---
title:    "C#: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil des Programmierens in C#. Es kann helfen, Fehler in Ihren Programmen zu vermeiden und sicherzustellen, dass Sie mit den richtigen Dateien und Verzeichnissen arbeiten.

## Wie geht das?

Um zu überprüfen, ob ein Verzeichnis existiert, können Sie die `Directory.Exists` Methode verwenden. Diese Methode gibt einen booleschen Wert zurück, der angibt, ob das Verzeichnis vorhanden ist oder nicht.

```C#
if (Directory.Exists(@"C:\Users\Benutzername\Dokumente"))
{
    Console.WriteLine("Das Verzeichnis existiert.");
}
else
{
    Console.WriteLine("Das Verzeichnis existiert nicht.");
}
```

Die Ausgabe dieses Codes wird je nachdem, ob das Verzeichnis existiert oder nicht, entweder "Das Verzeichnis existiert." oder "Das Verzeichnis existiert nicht." sein.

## Deep Dive

Die `Directory.Exists` Methode verwendet die Win32 API-Funktion `GetFileAttributes`, um zu überprüfen, ob das angegebene Verzeichnis vorhanden ist. Es ist jedoch wichtig zu beachten, dass diese Methode keine Unterscheidung zwischen Dateien und Verzeichnissen macht, sondern nur überprüft, ob der angegebene Pfad existiert oder nicht.

## Siehe auch

- [MSDN Dokumentation zur Directory.Exists Methode](https://docs.microsoft.com/de-de/dotnet/api/system.io.directory.exists)
- [Beispielcode für die Verwendung von Directory.Exists in C#](https://www.c-sharpcorner.com/article/how-to-check-if-directory-already-exists-using-C-Sharp/)
- [Interface für die Win32 API-Funktion GetFileAttributes](https://docs.microsoft.com/de-de/windows/desktop/api/fileapi/nc-fileapi-lpgetfileattributesa)