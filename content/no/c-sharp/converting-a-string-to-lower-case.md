---
title:                "C#: Konvertering av streng til små bokstaver"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med tekst og strenger i C#, kan det være nyttig å kunne konvertere dem til små bokstaver. Dette kan være nyttig for å sammenligne tekst uten å ta hensyn til store eller små bokstaver, eller for å gjøre søk enklere ved å sikre at både søketeksten og strengene er i samme case. Det er derfor viktig å vite hvordan man kan konvertere en streng til små bokstaver i C#.

## Hvordan

Det er enkelt å konvertere en streng til små bokstaver i C# ved hjelp av den innebygde "ToLower()" metoden. Denne metoden tar ingen argumenter og returnerer en ny streng med alle bokstavene konvertert til små bokstaver. Her er et eksempel på hvordan dette kan gjøres:

```C#
string tekst = "DEtte Er En Tekst";
string nyTekst = tekst.ToLower();
Console.WriteLine(nyTekst);
```

I dette eksempelet vil "nyTekst"variabelen inneholde "dette er en tekst" når den blir utskrevet til konsollen.

Det er også verdt å merke seg at denne metoden tar hensyn til ulike språk og bokstaver som bruker accent markeringer, og vil konvertere dem til sine tilsvarende små bokstaver.

## Dypdykk

For å forstå hvordan konverteringen av strenger til smale bokstaver fungerer i C#, må vi først forstå konseptet med Unicode og UTF-8. Unicode er en standard for å representere karakterer fra ulike språk og alfabet, mens UTF-8 er et format for å kode disse karakterene til bits og bokstaver som kan lagres og utveklses på en datamaskin.

Alle bokstaver og symboler i C# er representert som Unicode-tegn, og når vi bruker "ToLower()" metoden, kjører den en bakgrunnsprosess for å konvertere disse tegnene til sine tilsvarende små bokstaver ved hjelp av UTF-8 formatet.

Det er også viktig å huske at "ToLower()" metoden ikke bare konverterer store bokstaver til små, men den kan også håndtere bokstaver som allerede er i smått og beholde dem uendret.

## Se Også

- [MSDN om "ToLower()" metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [C# Strenger (Strings)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)