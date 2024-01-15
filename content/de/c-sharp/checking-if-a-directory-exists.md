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

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Bestandteil beim Programmieren. Es bietet die Möglichkeit, einen bestimmten Pfad aufzuzeigen und zu überprüfen, ob er vorhanden ist oder nicht. Dies ist besonders nützlich, wenn man beispielsweise Dateien in einem bestimmten Verzeichnis lesen oder schreiben möchte.

## Wie das gemacht wird

Eine Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung der `Directory.Exists()` Methode in C#. Diese Methode gibt einen booleschen Wert zurück und zeigt somit an, ob das angegebene Verzeichnis vorhanden ist oder nicht.

Beispielcode:

```C#
string path = @"C:\Beispiel\Verzeichnis";
if (Directory.Exists(path))
{
    Console.WriteLine("Das Verzeichnis existiert!");
}
else
{
    Console.WriteLine("Das Verzeichnis existiert nicht.");
}
```

Ausgabe:

```
Das Verzeichnis existiert!
```

## Tiefer Einblick

Um tiefer in das Thema einzutauchen, ist es wichtig zu verstehen, dass `Directory.Exists()` nicht nur überprüft, ob ein Verzeichnis existiert, sondern auch überprüft, ob der angegebene Pfad ein Verzeichnis ist. Wenn der Pfad auf eine Datei oder einen ungültigen Pfad zeigt, wird die Methode `false` zurückgeben.

Es ist auch wichtig zu beachten, dass diese Methode nur überprüft, ob ein Verzeichnis im lokalen Dateisystem vorhanden ist. Wenn man auf ein Verzeichnis in einem Netzwerk zugreifen möchte, muss man eine andere Methode verwenden, wie z.B. `Path.GetFullPath()` oder `Path.GetDirectoryName()`.

Es ist ratsam, vor dem Zugriff auf ein Verzeichnis immer zu überprüfen, ob es vorhanden ist, um Fehler zu vermeiden und die Benutzerfreundlichkeit zu verbessern.

## Siehe auch

- [Microsoft Dokumentation zu Directory.Exists-Methode](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Stack Overflow Beitrag zum Überprüfen von Netzwerkverzeichnissen in C#](https://stackoverflow.com/questions/953659/how-do-i-check-if-a-directory-exists-in-a-remote-directory)