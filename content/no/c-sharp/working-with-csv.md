---
title:                "Arbeid med csv"
html_title:           "C#: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# Hvorfor
Hvis du noen gang har håndtert store mengder data, har du sannsynligvis støtt på CSV-filer. Disse komma-separerte filene er et vanlig format for å lagre og utveksle data, og ved å lære hvordan man jobber med dem, kan du effektivt håndtere og manipulere dataene dine.

## Hvordan

```C#
// Les fra en CSV-fil
var lines = File.ReadAllLines("data.csv");

// Loop gjennom hver linje
foreach (var line in lines)
{
    // Del opp linjen etter komma og lagre i en liste
    var data = line.Split(',');

    // Hent ut spesifikke data fra linjen
    var name = data[0];
    var age = int.Parse(data[1]);

    // Gjør noe med dataene
    Console.WriteLine($"Navn: {name}, Alder: {age}");
}

// Skrive til en CSV-fil
var newData = new List<string>() { "John,25", "Jane,30", "Bob,40" };

// Konvertere dataen til en streng og skrive til filen
var dataString = string.Join("\n", newData);
File.WriteAllText("new_data.csv", dataString);

// Eksempel på utdata:
// Navn: John, Alder: 25
// Navn: Jane, Alder: 30
// Navn: Bob, Alder: 40
```

## Deep Dive

CSV-filer kan være mer komplekse enn bare en liste med data. De kan også inneholde overskrifter, ulike datatyper og til og med andre tegn enn bare komma for å separere dataene. Det er derfor viktig å ha god kunnskap om hvordan du kan håndtere ulike situasjoner når du jobber med CSV-filer.

En måte å jobbe med CSV-filer på er å bruke et bibliotek som heter CsvHelper. Dette biblioteket gir deg mange nyttige funksjoner for å lese og skrive til CSV-filer, håndtere ulike formater og feil, og også muligheten til å mappe dataene dine til objekter.

Det kan også være lurt å være oppmerksom på hvordan du håndterer store CSV-filer for å unngå problemer med hukommelsen. En måte å gjøre dette på er å lese inn og behandle dataene linje for linje i stedet for å lese hele filen inn i minnet.

## Se Også
- [CsvHelper bibliotek](https://joshclose.github.io/CsvHelper/)
- [Microsofts dokumentasjon om å jobbe med CSV-filer i C#](https://docs.microsoft.com/en-us/dotnet/api/system.data.csv?view=netcore-3.1)
- [Artikkel om beste praksis for å jobbe med store CSV-filer](https://www.codeproject.com/Articles/7467/A-Fast-CSV-Reader)