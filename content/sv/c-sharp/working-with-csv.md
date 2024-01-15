---
title:                "Arbeta med csv"
html_title:           "C#: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför
CSV är ett vanligt format för att lagra och dela data, och det är därför viktigt att kunna arbeta med det effektivt. Genom att lära dig hantera CSV-data i C# kan du enkelt importera och exportera data från olika program och verktyg.

## Hur man gör
För att arbeta med CSV i C# behöver du först inkludera namespace för CSV-hanteringsbiblioteket. Detta görs genom att skriva följande kod i början av ditt program:

```C#
using System.IO;
using CsvHelper;
```
För att läsa in en CSV-fil i ditt program behöver du först skapa en instans av CsvReader-klassen och ange filvägen för din CSV-fil:

```C#
using (StreamReader sr = new StreamReader("path/to/csvfile.csv"))
{
    using (var reader = new CsvReader(sr))
    {
        //läs in alla rader i CSV-filen
        var records = reader.GetRecords<RecordClass>();
    }
}
```
Som du kan se i koden ovan behöver du skapa en klass som representerar datastrukturen i din CSV-fil. I mitt exempel har jag använt RecordClass för detta ändamål. När du har läst in data från din CSV-fil kan du sedan använda den i ditt program som du önskar.

Om du vill skapa en CSV-fil från ditt program kan du använda CsvWriter-klassen på ett liknande sätt:

```C#
using (var writer = new CsvWriter(Console.Out))
{
    //skriv ut kolumnnamn
    writer.WriteHeader<RecordClass>();

    //skriv ut data
    writer.WriteRecords(records); 
}
```

## Deep Dive
Det finns flera olika sätt att anpassa hur CSV-filer läses in och skrivs ut i C#. Till exempel kan du ändra separator-tecken (standardmässigt är det komma) och du kan också ställa in olika kolumntyper för din data, till exempel som sträng eller som ett specifikt datatyp som du har definierat i din programkod.

För att läsa mer om alla de olika sätten att anpassa CSV-hantering i C#, besök dokumentationen för CsvHelper-biblioteket [här](https://joshclose.github.io/CsvHelper/).

## Se även
- [Microsofts officiella dokumentation för arbetande med CSV i C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [En guide för hur man läser och skriver CSV-filer i C#](https://dev.to/mkalam/how-to-read-and-write-csv-files-vf2)
- [En tutorial för hantering av CSV-data i C# med CsvHelper](https://dotnetthoughts.net/csvhelper-tutorial/)