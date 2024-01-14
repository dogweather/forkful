---
title:                "C#: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es viele Situationen, in denen es wichtig ist zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist. Zum Beispiel, um sicherzustellen, dass die Dateien, auf die Sie zugreifen möchten, auch wirklich existieren. In diesem Blog-Beitrag werde ich Ihnen zeigen, wie Sie in C# ganz einfach überprüfen können, ob ein Verzeichnis existiert.

## Wie geht's

Um zu überprüfen, ob ein Verzeichnis existiert, verwenden wir die Methode `Directory.Exists` aus der Klasse `System.IO`. Diese Methode gibt einen booleschen Wert zurück, der angibt, ob das angegebene Verzeichnis existiert oder nicht.

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

Das obige Beispiel zeigt die Verwendung der `Directory.Exists`-Methode, um zu überprüfen, ob das Verzeichnis mit dem Pfad `C:\Users\Benutzer\Desktop\Beispielverzeichnis` existiert. Wenn das Verzeichnis existiert, wird die Nachricht "Das Verzeichnis existiert." ausgegeben, andernfalls die Nachricht "Das Verzeichnis existiert nicht.".

## Tiefer ins Detail

Um zu verstehen, wie die `Directory.Exists`-Methode funktioniert, schauen wir uns den Quellcode an. Die Methode besteht aus nur einer Zeile:

```C#
public static bool Exists(string path)
{
    return Directory.InternalExists(path);
}
```

Wie Sie sehen können, ruft die `Directory.Exists`-Methode einfach die interne Methode `Directory.InternalExists` auf und gibt deren Rückgabewert zurück. Diese interne Methode durchsucht das Dateisystem und überprüft, ob das angegebene Verzeichnis existiert.

Es ist wichtig zu beachten, dass die `Directory.Exists`-Methode nur überprüft, ob das Verzeichnis existiert und nicht, ob es ein gültiges oder verfügbares Verzeichnis ist. Auch wenn das Verzeichnis aus irgendeinem Grund nicht zugänglich ist, wird die Methode immer `true` zurückgeben, solange das Verzeichnis existiert.

## Siehe auch

- [Directory.Exists-Methode in der Microsoft-Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.io.directory.exists)
- [Weitere Informationen zu Datei- und Verzeichnisoperationen in C#](https://www.c-sharpcorner.com/uploadfile/mahesh/file-and-directory-operations-in-C-Sharp/)

Vielen Dank, dass Sie meinen Blog-Beitrag gelesen haben. Ich hoffe, er war hilfreich für Sie. Wenn Sie weitere Fragen haben, zögern Sie nicht, einen Kommentar zu hinterlassen. Bis zum nächsten Mal!