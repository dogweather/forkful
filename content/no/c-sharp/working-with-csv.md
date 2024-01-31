---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV står for Comma-Separated Values - enkle tekstfiler som inneholder tabellinfo, linje for linje. Programmerere bruker CSV fordi det er et lettvekt-format for datautveksling, som er enkelt å lese og skrive både for mennesker og maskiner.

## Slik gjør du:
Lesing av CSV:
```C#
using System;
using System.IO;

string csvFilePath = @"dinCsvFil.csv";

foreach (var line in File.ReadLines(csvFilePath))
{
    var values = line.Split(',');
    // Antar ingen komma inni selve dataene
    Console.WriteLine($"Navn: {values[0]}, Alder: {values[1]}");
}
```
Skriving til CSV:
```C#
using System.IO;

string csvFilePath = @"dinCsvFil.csv";
using (var writer = new StreamWriter(csvFilePath))
{
    writer.WriteLine("Navn,Alder");
    writer.WriteLine("Ola,42");
    writer.WriteLine("Kari,36");
}
```
Test output etter skriving vil være en fil `dinCsvFil.csv` med innholdet:
```
Navn,Alder
Ola,42
Kari,36
```

## Dykk dypere
CSV er gammelt, helt tilbake til det tidlige 1970-årene. Alternativer som XML og JSON er ofte bedre for komplekse data, men CSV vinner på enkelhet. Det viktigste er å håndtere variasjon i format, som anførselstegn rundt tekst med komma og newline-karakterer inni verdier. .NET's `TextFieldParser` eller tredjepartsbiblioteker som CsvHelper kan hjelpe til med dette.

## Se også
- Microsoft TextFieldParser dokumentasjon: https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.fileio.textfieldparser
- CsvHelper bibliotek: https://joshclose.github.io/CsvHelper/
- RFC 4180, standard for CSV-format: https://www.rfc-editor.org/rfc/rfc4180.txt

Disse kildene gir mer info og hjelper deg å gripe ansvaret for dine CSV-operasjoner.
