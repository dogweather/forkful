---
title:                "C#: Konvertere dato til en streng"
simple_title:         "Konvertere dato til en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor ville noen ønske å konvertere en dato til en streng? Vel, datoer er en essensiell del av programmering. De brukes til å følge med tiden og organisere data. Å konvertere en dato til en streng kan være nyttig for å presentere informasjon på en mer lesbar måte for brukeren.

## Hvordan
Konvertering av en dato til en streng er enkelt å gjøre i C#. Du kan bruke innebygde metoder for å konvertere en dato til en streng eller bruke en egen formateringskode. Her er et eksempel på begge metodene:

```C#
// Konverter dato til streng ved hjelp av innebygde metoder
DateTime dato = DateTime.Now;
string datoSomStreng = dato.ToString();

// Konverter dato til streng med egen formateringskode
string datoMedFormat = dato.ToString("dd/MM/yyyy");
```

Output:

```
22/11/2021
```

```
Tuesday, November 22, 2021
```

Den innebygde ToString() metoden returnerer en standard formatert streng for datoen, mens medformateringskoden kan tilpasses for å vise datoen på en spesifikk måte.

## Dypdykk
Det finnes en rekke forskjellige formateringskoder som kan brukes til å konvertere en dato til en streng i C#. Noen av de vanligste inkluderer:

- "dd" for å vise bare dagnummeret
- "MM" for å vise bare månedsnummeret
- "yyyy" for å vise hele årstallet
- "ddd" for å vise den forkortede versjonen av ukedagen (f.eks. "man" for mandag)
- "MMMM" for å vise den fulle navnet på måneden

Det er også mulig å angi et bestemt format ved å bruke ToString() metoden og en CultureInfo, for eksempel for å vise datoen på et annet språk.

## Se også
- [Microsoft Docs: DateTime.ToString() metode](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Microsoft Docs: Standard Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [C# DateTime Format Examples](https://www.csharp-examples.net/string-format-datetime/)