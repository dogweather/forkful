---
title:    "C#: Eine Textdatei lesen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum das Lesen von Textdateien in der C# Programmierung wichtig sein kann. Zum Beispiel können Textdateien verwendet werden, um Konfigurationsdaten zu speichern oder um Protokolle zu erstellen. Sie ermöglichen es auch, Daten in einem einfachen, menschenlesbaren Format zu speichern, das von anderen Programmen leichter verarbeitet werden kann.

## Wie man es macht
Wenn Sie eine Textdatei in C# lesen möchten, gibt es ein paar Schritte, die Sie befolgen müssen:

1. Öffnen Sie die Datei mit der `StreamReader` Klasse.
2. Lesen Sie Zeilenweise mit der `ReadLine()` Methode.
3. Speichern Sie die eingelesenen Daten in geeigneten Variablen.
4. Schließen Sie die Datei mit der `Close()` Methode.

Hier ist ein Beispielcode, der eine Textdatei mit Namen "beispiel.txt" liest und jede Zeile ausgibt:

```C#
using System;
using System.IO;

namespace BeispielLesen
{
    class Program
    {
        static void Main(string[] args)
        {
           StreamReader reader = new StreamReader("beispiel.txt");
           string line;

           while ((line = reader.ReadLine()) != null)
           {
               Console.WriteLine(line);
           }

           reader.Close();
        }
    }
}
```

Die Ausgabe dieses Codes würde wie folgt aussehen, wenn die Textdatei folgende Zeilen enthält:

```
Hallo
Wie geht es dir?
Ich hoffe, du hast einen schönen Tag!
```

```
Hallo
Wie geht es dir?
Ich hoffe, du hast einen schönen Tag!
```

## Tiefere Einblicke
Obwohl das Lesen einer einfachen Textdatei in C# nicht kompliziert ist, gibt es einige Dinge, auf die man achten sollte:

- Stellen Sie sicher, dass die Textdatei im richtigen Verzeichnis liegt oder geben Sie den vollständigen Pfad zur Datei in Ihrem Code an.
- Achten Sie darauf, die Datei mit der `Close()` Methode zu schließen, um sicherzustellen, dass alle Ressourcen freigegeben werden.
- Die `ReadLine()` Methode gibt `null` zurück, wenn die End-of-File (EOF) erreicht ist. Stellen Sie daher sicher, dass Ihre Schleife aufhört, wenn dies geschieht, um Fehler zu vermeiden.

## Siehe auch
- Mehr über das Lesen von Textdateien in C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file
- Weitere Informationen zur `StreamReader` Klasse: https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader