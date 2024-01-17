---
title:                "Überprüfen ob ein Verzeichnis existiert"
html_title:           "C#: Überprüfen ob ein Verzeichnis existiert"
simple_title:         "Überprüfen ob ein Verzeichnis existiert"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine gängige Programmieraufgabe, bei der geprüft wird, ob ein bestimmter Ordner auf einem Speicherort vorhanden ist. Es wird häufig von Entwicklern verwendet, um die Anwesenheit von spezifischen Dateien oder Ordnern zu überprüfen, bevor sie mit ihrem Programm fortfahren.

## Wie geht das?
```C#
if (Directory.Exists(path))
{
    Console.WriteLine("Das Verzeichnis existiert.");
}
else
{
    Console.WriteLine("Das Verzeichnis existiert nicht.");
}
```
Das obige Beispiel nutzt die statische Methode "Directory.Exists" aus der Klasse "Directory", um zu überprüfen, ob das angegebene Verzeichnis mit dem Pfad "path" existiert. Wenn ja, wird eine Erfolgsmeldung ausgegeben, andernfalls eine Fehlermeldung.

## Tiefentauchen
Das Überprüfen von Verzeichnissen hat eine lange Geschichte in der Programmierung. In Zeiten, in denen Speicherplatz teurer war und Dateien sorgfältig organisiert werden mussten, war es wichtig zu wissen, ob ein bestimmtes Verzeichnis bereits vorhanden war, um Duplikate zu vermeiden oder bestimmte Aktionen auszuführen. Heutzutage gibt es zwar bessere Methoden, um Dateien und Ordner zu verwalten, aber das Überprüfen von Verzeichnissen ist immer noch eine nützliche und häufig verwendete Funktion.

Es gibt auch alternative Möglichkeiten, um zu überprüfen, ob ein Verzeichnis existiert, wie beispielsweise das Verwenden der Klasse "File", um den spezifischen Pfad zu überprüfen. Die Verwendung der "Directory" Klasse ist jedoch die gängigste und einfachste Methode.

## Sieh auch
- [Microsoft Dokumentation zum Überprüfen von Verzeichnissen in C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [Tutorial für Einsteiger zum Arbeiten mit Verzeichnissen in C#](https://www.tutorialspoint.com/csharp/csharp_working_with_directories.htm)
- [Beispielcode für die Verwendung der "Directory.Exists" Methode](https://www.codegrepper.com/code-examples/csharp/c%23+checking+if+directory+exists)