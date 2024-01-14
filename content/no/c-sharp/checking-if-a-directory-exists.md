---
title:                "C#: Sjekke om en mappe eksisterer"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om et katalog eksisterer er en viktig del av programmering. Det gjør det mulig for oss å håndtere ulike tilfeller som kan oppstå når vi jobber med våre programmer. Ved å sjekke om en katalog eksisterer, kan vi håndtere feil på en trygg og effektiv måte.

## Hvordan

Vi kan enkelt sjekke om en katalog eksisterer ved å bruke metoden `Directory.Exists()` i C#. Dette er en innebygd funksjon som tar inn en streng som representerer katalogens bane, og returnerer en boolsk verdi som indikerer om katalogen eksisterer eller ikke.

```C#
// Sjekker om katalogen "Documents" eksisterer i "Min datamaskin"
bool exists = Directory.Exists(@"C:\Min datamaskin\Documents");

// Utskrift av resultatet
Console.WriteLine(exists);

// Resultat: True
```

## Dypdykk

Når vi bruker `Directory.Exists()` metoden, er det viktig å merke seg at den også vil returnere `true` hvis katalogen er en filbane og ikke en mappe. Dette kan føre til uventede resultater hvis vi forventer at katalogen skal være en mappe.

En annen ting å merke seg er at denne metoden bare sjekker for eksistensen av en katalog og ikke nødvendigvis om vi har tilgang til den. Det er derfor viktig å sørge for å håndtere eventuelle unntak som kan oppstå.

Vi kan også bruke `Directory.GetDirectories()` og `Directory.GetFiles()` metoder til å hente en liste over kataloger og filer i en angitt bane. Disse metodene vil også kaste unntak hvis vi ikke har tilgang til den angitte banen.

## Se også

- [Microsoft dokumentasjon om Directory.Exists() metoden](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [C# Directory.Exists() eksempel på GeeksforGeeks](https://www.geeksforgeeks.org/check-if-a-directory-exists-in-a-path-in-c-sharp/)
- [Se om jeg har tilgang til en fil eller katalog med C# blogginnlegg på norsk](https://blogg.example.com/se-om-jeg-har-tilgang-til-en-fil-eller-katalog-med-c-sharp.html)