---
title:                "C#: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
CSV eller "Comma-Separated Values" er en vanlig filformat som brukes til å lagre tabellinformasjon, for eksempel data fra en database eller et regneark. Derfor er det viktig for programmerere å vite hvordan man skal jobbe med CSV-filer for å kunne håndtere og manipulere dataene på en effektiv måte.

## Hvordan
For å jobbe med CSV-filer i C# kan du bruke nugget-pakken "CsvHelper", som gjør det enkelt å lese og skrive CSV-filer.

Først må du legge til CsvHelper-pakken i prosjektet ditt ved å åpne Package Manager Console og skrive følgende kommando:

```C#
PM> Install-Package CsvHelper -Version 27.0.1
```

Når pakken er installert, kan du begynne å bruke den i koden din. For å lese data fra en CSV-fil, må du opprette en "CsvReader" og angi stien til CSV-filen som du vil lese fra:

```C#
using (var reader = new CsvReader(new StreamReader("minfil.csv"), CultureInfo.InvariantCulture))
{
    // Les data fra CSV-filen og lagre den i en liste av "Record"-objekter
    var records = reader.GetRecords<Record>().ToList();

    // Gjør noe med dataene her, for eksempel skriv ut dem
    foreach(var record in records)
    {
        Console.WriteLine($"{record.Id} - {record.Name}");
    }
}
```

For å skrive data til en CSV-fil, må du opprette en "CsvWriter" og angi stien til CSV-filen som du vil skrive til:

```C#
using (var writer = new CsvWriter(new StreamWriter("minfil.csv"), CultureInfo.InvariantCulture))
{
    // Skriv dataene til CSV-filen
    writer.WriteRecords(records);
}
```

Det er også mulig å tilpasse hvordan dataene blir lest og skrevet ved å bruke attributter i klassedefinisjonen til objektene dine. For eksempel kan du angi hvilken kolonnedelimiterer som blir brukt i filen, og hvilken formatering som skal brukes for å konvertere datatyper.

## Dykk dypere
En av fordelene med å bruke CsvHelper er at den håndterer konvertering av datatyper automatisk. Dette betyr at du ikke trenger å bekymre deg for å konvertere dataene manuelt fra tekst til riktig format. Du kan også bruke LINQ for å gjøre komplekse operasjoner på dataene, som for eksempel å filtrere eller sortere dem.

Det er også verdt å nevne at CsvHelper støtter flere forskjellige formater, og at du kan konfigurere den til å håndtere spesielle tegn eller språkkoder.

## Se også
- [CsvHelper dokumentasjon](https://joshclose.github.io/CsvHelper/)
- [Offisiell Microsoft-dokumentasjon for å jobbe med CSV-filer i C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)