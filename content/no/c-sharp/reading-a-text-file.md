---
title:    "C#: Lesing av tekstfil"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å lære å lese tekstfiler i C#. Det kan være nyttig for å behandle store mengder data, som f.eks. loggfiler, CSV-filer eller XML-filer. Det kan også være nyttig for å analysere og manipulere data som skal brukes i et program.

## Hvordan gjøre det

For å lese en tekstfil i C#, må du først åpne filen ved hjelp av `File.OpenText()` metoden. Deretter kan du lese linjene i filen ved å bruke `StreamReader`-klassen. Her er et eksempel på hvordan du kan gjøre det:

```C#
var path = @"C:\min-fil.txt";
using (var reader = File.OpenText(path))
{
    string line;
    while ((line = reader.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}
```

Dette vil åpne filen "min-fil.txt" og skrive ut hver linje i konsollen. Du kan også bruke `File.ReadAllText()` metoden for å lese hele filen som en streng.

## Dypdykk

Det er flere ting å være oppmerksom på når du leser tekstfiler i C#. Først og fremst er det viktig å lukke filen når du er ferdig med å lese den, ved å bruke `using`-blokker, som i eksempelet ovenfor. Dette sørger for at filen blir lukket riktig selv om det oppstår en feil under lesingen.

Det er også viktig å vite at tekstfiler kan ha forskjellige tegnsett, og det kan være nødvendig å angi riktig tegnsett når du åpner filen ved hjelp av `File.OpenText()` metoden. Dette gjøres ved å bruke en `Encoding`-parameter.

## Se også

- [C# Dokumentasjon for File.OpenText() metoden](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.opentext)
- [C# Dokumentasjon for StreamReader-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [C# Dokumentasjon for File.ReadAllText() metoden](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)