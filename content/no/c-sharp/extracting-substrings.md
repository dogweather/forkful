---
title:                "C#: Uttrekking av substringer"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å hente ut delstrenger fra tekststrenger kan være nyttig når du jobber med tekstbehandling i C#. Dette kan hjelpe deg med å håndtere og manipulere tekst på en mer effektiv måte.

## Hvordan gjøre det

For å hente ut en delstreng fra en tekststreng i C#, kan du bruke metoden `Substring()`. Denne metoden tar to parametere - startindeks og lengden på delstrengen du ønsker å hente ut.

```C#
string tekst = "Dette er en tekststreng";
string delstreng = tekst.Substring(5, 2); // 5 er startindeksen til "er" og 2 er lengden på delstrengen
Console.WriteLine(delstreng); // Output: er
```

Du kan også bruke `Substring()`-metoden til å hente ut en del av en tekststreng basert på en gitt indeks, uten å spesifisere lengden på delstrengen.

```C#
string tekst = "Dette er en tekststreng";
string delstreng = tekst.Substring(11); // 11 er indeksen til "tekststreng"
Console.WriteLine(delstreng); // Output: tekststreng
```

## Dypdykk

Når du bruker `Substring()`-metoden, er det viktig å huske på at indeksene starter med 0. Dette betyr at første tegn i en tekststreng vil ha indeksen 0, det neste vil ha indeksen 1, og så videre.

En annen viktig ting å merke seg er at `Substring()`-metoden ikke endrer den opprinnelige tekststrengen, men returnerer en ny tekststreng med den utvalgte delen. Derfor må du lagre den returnerte verdien i en ny variabel eller bruke den umiddelbart.

## Se også

- [C# String.Substring() metode (på engelsk)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [C# Tutorials (på engelsk)](https://www.tutorialspoint.com/csharp/index.htm)
- [C# String Handling (på engelsk)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)