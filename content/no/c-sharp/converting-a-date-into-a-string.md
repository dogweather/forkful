---
title:                "C#: Konvertere dato til tekststreng"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig oppgave i mange programmeringsspråk, inkludert C#. Dette er ofte nødvendig for å vise datoer på en leservennlig måte eller for å lagre dem i en database. I denne bloggposten vil vi gå gjennom hvordan du kan utføre denne oppgaven i C#.

## Slik gjør du det

For å konvertere en dato til en streng i C#, kan du bruke den innebygde metoden `ToString()` i `DateTime`-klassen. Denne metoden tar imot et formatargument og returnerer en streng basert på dette formatet. Her er et eksempel på hvordan du kan bruke `ToString()` for å konvertere en dato til en streng:

```C#
Console.WriteLine(DateTime.Now.ToString("dd.MM.yyyy"));
```

Dette vil vise dagens dato i formatet "dd.MM.yyyy" (for eksempel 25.08.2021). Du kan også velge andre formatalternativer, for eksempel "MM/dd/yyyy" eller "dddd, d MMMM yyyy" for å få en mer detaljert dato.

Det er også mulig å inkludere klokkeslett i strengen ved å bruke formatet "dd.MM.yyyy HH:mm:ss". Det finnes en rekke forskjellige formatalternativer, så det er lurt å se på dokumentasjonen til Microsoft for å finne det som passer best for ditt formål.

## Dykk dypere

Det finnes også andre måter å konvertere en dato til en streng i C#, som for eksempel å bruke `ToString()`-metoden i `DateTimeOffset`-klassen eller å bruke `ToString()` i en kundeimplementert `IFormattable`-klasse. Det er også mulig å formatere datoen ved hjelp av `string.Format()`-metoden.

Det er viktig å være bevisst på formatering av datoer i ulike kulturer. Hvis du ønsker å formatere datoer basert på brukerens språk og kultur, kan du bruke `CultureInfo`-klassen og dens metoder som `ToString()`. Det er også viktig å håndtere eventuelle unntak som kan oppstå ved konvertering, for eksempel hvis formatet ikke er gyldig.

## Se også

- MSDN-dokumentasjon for `DateTime.ToString()`-metoden: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring
- Oversikt over tilgjengelige formatalternativer for datoer i C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
- Eksempler på formattering av datoer basert på kulturelle innstillinger: https://www.c-sharpcorner.com/UploadFile/0f68f2/date-and-time-format-in-C-Sharp/