---
title:                "C#: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum: 

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil der C# Programmierung, da es sicherstellt, dass die Anwendung richtig auf Dateisystemänderungen reagieren kann.

## Wie:

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die "Directory.Exists" Methode verwenden. Sie nimmt eine Zeichenfolge als Parameter, die den Pfad des zu überprüfenden Verzeichnisses enthält.

```C#
if(Directory.Exists(@"C:\Benutzer\Kunde\Ordner")) 
{
    Console.WriteLine("Das Verzeichnis existiert!");
}
else 
{
    Console.WriteLine("Das Verzeichnis existiert nicht!");
}

// Ausgabe: Das Verzeichnis existiert!
```

## Deep Dive:

Bei der Überprüfung des Verzeichnisses wird die "Directory.Exists" Methode eine boolesche Rückgabe ausgeben, die angibt, ob das Verzeichnis existiert oder nicht. Wenn das Verzeichnis nicht existiert, wird die Methode "false" zurückgeben. Dies kann hilfreich sein, um entsprechend auf fehlende Verzeichnisse zu reagieren.

Darüber hinaus ermöglicht die Verwendung dieser Methode mit dem "if" Statement eine einfachere Kontrolle des Programmflusses, indem sie es uns ermöglicht, bestimmte Anweisungen auszuführen, wenn das Verzeichnis existiert.

# Siehe auch:

- [Directory.Exists Methode Dokumentation von Microsoft](https://docs.microsoft.com/de-DE/dotnet/api/system.io.directory.exists?view=net-5.0)
- [C# Pfad-Klasse Dokumentation von Microsoft](https://docs.microsoft.com/de-DE/dotnet/api/system.io.path?view=net-5.0)