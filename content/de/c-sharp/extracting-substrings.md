---
title:                "C#: Substring extrahieren"
simple_title:         "Substring extrahieren"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist ein wichtiger Prozess in der Programmierung, der es uns ermöglicht, Teile von Strings, die wir möglicherweise nicht benötigen, zu entfernen oder zu bearbeiten. Es ist eine nützliche Technik, die uns in der Lage stellt, Strings effizienter zu verarbeiten und unsere Codes übersichtlicher zu gestalten.

## Wie

Um eine Substring in C# zu extrahieren, können wir die Methode `Substring()` verwenden. Diese Methode erfordert zwei Parameter: die Startposition und die Anzahl der Zeichen, die wir extrahieren möchten. Hier ist ein Beispiel:

```C#
string str = "Hallo Welt";
string substr = str.Substring(6, 5); // Die Substring-Methode nimmt unsere Zeichen ab Index 6 (W) und extrahiert 5 Zeichen (Welt).

Console.WriteLine(substr); // Ausgabe: Welt
```

Wie Sie sehen können, wird der Substring "Welt" extrahiert und in der Variable `substr` gespeichert. Wir können auch die `Length`-Eigenschaft von Strings verwenden, um die Länge des Substrings dynamisch zu bestimmen. Hier ist ein Beispiel:

```C#
string str = "Hallo Welt";
string substr = str.Substring(6, str.Length - 6); // Ab Index 6 (W) extrahieren wir die restliche Länge des Strings (5 Zeichen).

Console.WriteLine(substr); // Ausgabe: Welt
```

Die `Substring()`-Methode ist auch nützlich, wenn wir Teile von Benutzereingaben oder von Dateinamen isolieren müssen. Zum Beispiel könnten wir einen Pfad zu einer Datei haben, aber nur den Dateinamen benötigen. In diesem Fall können wir die `LastIndexOf()`-Methode verwenden, um die Position des letzten Schrägstrichs (oder Rückwärtsschrägstrichs) zu finden und dann den Substring zu extrahieren. Hier ist ein Beispiel:

```C#
string path = "C:/Users/Benutzer/Bilder/MeinBild.jpg";
int lastIndex = path.LastIndexOf('/'); // Die Position der letzten Schrägstrich.
string filename = path.Substring(lastIndex + 1); // Der Substring beginnt ab dem Zeichen nach dem Schrägstrich.

Console.WriteLine(filename); // Ausgabe: MeinBild.jpg
```

## Deep Dive

Die `Substring()`-Methode kann auch in Kombination mit anderen Methoden verwendet werden, um komplexe Extraktionen durchzuführen. Zum Beispiel könnten wir anstelle der `LastIndexOf()`-Methode die `Split()`-Methode verwenden, um den Pfad an jedem Schrägstrich aufzuteilen und dann den letzten Teil (Dateiname) zu extrahieren. Wir können auch die `Replace()`-Methode verwenden, um bestimmte Zeichen in einem Substring zu entfernen oder zu ersetzen. Hier ist ein Beispiel:

```C#
string path = "C:/Users/Benutzer/Bilder/MeinBild.jpg";
string[] parts = path.Split('/'); // Der Pfad wird an jedem Schrägstrich aufgeteilt.
string filename = parts[parts.Length - 1]; // Der letzte Teil des Strings wird extrahiert.

string reversedFilename = filename.Replace("Bild", "dliaB"); // Der Substring "Bild" wird durch "dliaB" ersetzt.
Console.WriteLine(reversedFilename); // Ausgabe: MeinDliab.jpg
```

Es gibt unendlich viele Möglichkeiten, Substrings effektiv zu extrahieren. Es ist wichtig, dass wir uns mit den verschiedenen Methoden und Techniken in C# vertraut machen, um sie in unseren Codes effizient nutzen zu können.

## Siehe auch

- [String.Substring Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.string.substring)
- [String.Length Eigenschaft (System)](https://docs.microsoft.com/de-de/dotnet/api/system.string.length)
- [String.LastIndexOf Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.string.lastindexof)
- [String.Split Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.string.split)
- [String.Replace Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.string.replace)