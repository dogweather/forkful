---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:03.449068-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, einen Text in ein `DateTime`-Objekt umzuwandeln. Wir machen das, weil Daten und Zeiten oft als Text aus Dateien, Benutzereingaben oder APIs kommen und für Datumsoperationen in einem standardisierten Format vorliegen müssen.

## How to:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "24.12.2023";
        string format = "dd.MM.yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;
        
        try
        {
            DateTime parsedDate = DateTime.ParseExact(dateString, format, provider);
            Console.WriteLine(parsedDate.ToString("d")); // Gibt das Datum aus: 24.12.2023
        }
        catch (FormatException)
        {
            Console.WriteLine("Ungültiges Datum.");
        }
    }
}
```

## Deep Dive
Das Parsen von Datumsangaben ist wichtig, da verschiedene Kulturen verschiedene Formate verwenden. Deshalb bietet .NET `CultureInfo`, um Missverständnisse zu vermeiden. Vor .NET gab es mehr Arbeit, da Entwickler eigene Parser schreiben mussten oder sich auf weniger flexible APIs verließen.

Alternativen zu `DateTime.ParseExact` sind `DateTime.TryParse` und `DateTime.TryParseExact`, die fehlertoleranter sind und `false` zurückgeben statt eine Ausnahme zu werfen. Für die Verarbeitung von Zeitstempeln in verschiedenen Formaten können wir `DateTimeOffset` verwenden.

Details der Implementierung: `ParseExact` verlangt ein genaues Format, sonst wirft es eine `FormatException`. Die `CultureInfo.InvariantCulture` hilft dabei, kulturunabhängig zu parsen, z.B. wenn wir wissen, das Datum ist immer im gleichen Format.

## Siehe Auch
- Microsoft Dokumentation zu `DateTime`: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1
- Über `CultureInfo`: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1
- Zum Thema Zeitformate in C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
