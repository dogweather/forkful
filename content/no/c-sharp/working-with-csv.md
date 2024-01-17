---
title:                "Å jobbe med CSV"
html_title:           "C#: Å jobbe med CSV"
simple_title:         "Å jobbe med CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

CSV (Comma Separated Values) eller kommaseparerte verdier er et format for å lagre og utveksle tabellignende data. Det brukes ofte til å lagre data som skal behandles eller analyseres av et dataprogram. Programmere bruker CSV fordi det er en enkel og effektiv måte å organisere og lagre data på.

## Slik gjør du:

CSV er et tekstformat som kan endres og leses av mennesker. Det er enkelt å lage og behandle med C#. Her er et eksempel på hvordan du leser en CSV-fil med C#:

```C#
using System;
using System.Collections.Generic;
using System.IO;

var reader = new StreamReader("example.csv");
List<List<string>> data = new List<List<string>>();

while (!reader.EndOfStream)
{
    string line = reader.ReadLine();
    List<string> values = line.Split(',').ToList();
    data.Add(values);
}

foreach (List<string> row in data)
{
    foreach (string value in row)
    {
        Console.Write(value + " ");
    }
    Console.WriteLine();
}
```

Dette eksempelet bruker StreamReader-klassen til å lese en CSV-fil og lagrer dataene i en listestruktur. Dataene kan deretter behandles videre etter behov. Her er et eksempel på output fra koden over:

```
Navn Alder Kjønn
Andrea 27 Kvinne
Markus 31 Mann
Sara 24 Kvinne
```

## Dypdykk:

CSV-formatet har eksistert siden 1972 og har vært et populært valg for å lagre og utveksle data. Alternativer som regneark og SQL-databaser har også blitt mer utbredt, men CSV forblir et praktisk valg for enkel datastrukturering og utveksling.

I tillegg til å lese og skrive CSV-filer, kan C# også generere CSV-filer ved å bruke CsvHelper-biblioteket. Dette gjør det enklere å håndtere store datamengder og spesifisere formatet på dataene.

## Se også:

- [CsvHelper dokumentasjon](https://joshclose.github.io/CsvHelper/)
- [En oversikt over CSV-formatet](https://en.wikipedia.org/wiki/Comma-separated_values)