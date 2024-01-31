---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för "Comma-Separated Values". Det är en enkel filformat där data separeras med kommatecken, vilket är smidigt för att lagra och utbyta enkla datatabeller mellan program.

## How to:
```C#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class CsvReader
{
    public static void Main()
    {
        string filePath = "exempel.csv";
        var data = ReadCsvFile(filePath);

        foreach (var row in data)
        {
            Console.WriteLine(string.Join(", ", row));
        }
    }

    public static List<List<string>> ReadCsvFile(string path)
    {
        var lines = File.ReadAllLines(path);
        var data = lines.Select(l => l.Split(',').ToList()).ToList();
        return data;
    }
}
```
Output:
```plaintext
Namn, Ålder, Stad
Alice, 29, Stockholm
Bob, 34, Göteborg
```

## Deep Dive
CSV-filer har använts sedan 1970-talet, ett enkelt format som kan skapas och läsas av kalkylprogram samt programmeringsspråk. Alternativ till CSV inkluderar JSON och XML, som båda kan hantera mer komplext data. När du arbetar med CSV i C#, se till att hantera olika kantfall som t.ex. kommatecken inuti ett värde eller radbyten inuti fält.

## See Also
- Microsofts dokumentation om `File` klassen: [File Class (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- Detaljer om CSV-formatet: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- För en mer avancerad CSV-läsning, se `CsvHelper` biblioteket: [CsvHelper (Github)](https://github.com/JoshClose/CsvHelper)
