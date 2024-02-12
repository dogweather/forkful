---
title:                "Datum in einen String umwandeln"
aliases: - /de/c-sharp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:14.305520-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Umwandeln eines Datums in einen String bedeutet, ein Datum von einem Format, das für die Datumsverarbeitung optimiert ist, in einen Text umzuwandeln. Programmierer machen das, um Daten benutzerfreundlich zu präsentieren oder sie in einem nicht-datumsbasierten System zu speichern.

## How to (Wie man's macht):
C# bietet die `ToString`-Methode für das `DateTime`-Objekt, um Datumsangaben in Strings umzuwandeln. Sehen wir uns das an:

```C#
using System;

public class DateFormatter
{
    public static void Main(string[] args)
    {
        DateTime now = DateTime.Now;
        string dateFormat1 = now.ToString("dd.MM.yyyy");
        string dateFormat2 = now.ToString("dddd, dd MMMM yyyy");
        
        Console.WriteLine(dateFormat1); // z.B. "05.04.2023"
        Console.WriteLine(dateFormat2); // z.B. "Mittwoch, 05 April 2023"
    }
}
```

## Deep Dive (Tiefere Einblicke):
Datum in String zu konvertieren, ist nichts Neues. Schon früh erkannten Programmierersprachen die Notwendigkeit, Daten menschenlesbar zu machen. C# hat das vereinfacht durch `DateTime.ToString()`. Es gibt Alternativen wie `String.Format()` oder Interpolation, die `ToString()` implizit aufrufen.

Details der Implementierung:

- Kulturabhängig: `ToString()` kann kulturspezifische Formatierungen nutzen, wie `en-US` oder `de-DE`.
- Geschwindigkeit: Direkte Methoden wie `ToString("yyyyMMdd")` sind meist schneller als `String.Format()`.
- Anpassbar: Mit eigenen Format-Strings lässt sich fast jedes gewünschte Format erzeugen.

## See Also (Siehe auch):
- MSDN Dokumentation zur `DateTime`-Klasse: [docs.microsoft.com/de-de/dotnet/api/system.datetime](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- Standard- und benutzerdefinierte Datums- und Zeitformatzeichenfolgen: [docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings](https://docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings)
- Darstellung von Daten und Uhrzeiten in unterschiedlichen Kulturen: [docs.microsoft.com/de-de/dotnet/standard/globalization-localization](https://docs.microsoft.com/de-de/dotnet/standard/globalization-localization)
