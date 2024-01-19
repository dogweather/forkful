---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei bezeichnet den Vorgang, eine Datei mit Daten zu generieren, die nur für die Dauer einer Sitzung oder eines Prozesses bestehen bleibt. Programmiere diese, um Daten zwischenspeichern zu können, ohne den permanenten Speicher zu belasten.

##Wie geht das?

Hier ist ein einfacher Weg, eine temporäre Datei in C# zu erstellen.

```C#
using System.IO;

public class TempDateiManager
{
    public string TempDateiErstellen()
    {
        string tempPfad = Path.GetTempFileName();
        File.WriteAllText(tempPfad, "Dies ist eine Test-Datei.");
        return tempPfad;
    }
}
```

Um die Datei zu lesen, führen Sie einfach diese Methode aus:

```C#
public void DateiLesen(string dateiPfad)
{
    string inhalt = File.ReadAllText(dateiPfad);
    Console.WriteLine(inhalt);
}
```

Die Eingabeparameter sind die Pfade der zu schreibenden und zu lesenden Dateien.

## Tiefere Informationen

Historisch gesehen entstand die Notwendigkeit temporärer Dateien aus der Notwendigkeit heraus, große Datenmengen zu verarbeiten, die der Arbeitsspeicher nicht alleine bewältigen konnte. In modernen Anwendungen erleichtert das Erstellen temporärer Dateien den Datentransfer zwischen verschiedenen Teilen eines Programms oder zwischen verschiedenen Programmen.

Alternativen zur Verwendung von temporären Dateien sind Datenstrukturen im Speicher wie Listen oder Arrays. Der Vorteil von temporären Dateien ist jedoch ihre Persistenz über den Arbeitsprozess hinaus, ihre größere Kapazität und die einfachere Zusammenarbeit zwischen unterschiedlichen Anwendungen.

Auf implementatorischer Ebene sollte beachtet werden, dass temporäre Dateien datenschutzrechtliche Fragen aufwerfen können. Sie sollten daher angemessen gesichert und nach Gebrauch gelöscht werden.

## Siehe auch

- [System.IO.Path.GetTempFileName Methode](https://docs.microsoft.com/de-de/dotnet/api/system.io.path.gettempfilename?view=net-6.0)
- [Datei- und Datenstrom-E/A](https://docs.microsoft.com/de-de/dotnet/standard/io/)
- [Wie man temporäre Dateien in C# verwaltet](https://www.codeproject.com/Articles/37946/Working-with-temp-files-in-C-Sharp)