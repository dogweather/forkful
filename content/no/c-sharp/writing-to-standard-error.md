---
title:                "C#: Skriving til standardfeil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne skrive til standardfeil er en viktig ferdighet i C# programmering. Det lar deg gi brukerne dine verdifull informasjon om feil og unntak som kan oppstå under kjøring av programmet ditt. Dette kan gjøre feilsøking og debugging mye enklere og mer effektivt.

## Hvordan

For å skrive til standardfeil i C#, kan du bruke Console.Error.WriteLine() metoden. Denne metoden tar inn en streng som argument og skriver den ut til standardfeilstrømmen. Her er et enkelt eksempel:

```C#
Console.Error.WriteLine("Dette er en feilmelding");
```

Når dette kjøres, vil strengen "Dette er en feilmelding" bli skrevet til standardfeilstrømmen, som vanligvis er konsollen. Merk at måten du skriver til standardfeil på er den samme som for standardutgang, men i stedet for Console.WriteLine() bruker du Console.Error.WriteLine().

Du kan også skrive til standardfeil ved å bruke Console.SetError()-metoden. Dette lar deg endre standardfeilstrømmen til en annen strøm, som for eksempel en fil eller en nettverkstilkobling. Her er et eksempel på hvordan dette kan gjøres:

```C#
// Opprett en filstrøm for standardfeil
FileStream errorStream = new FileStream("error.log", FileMode.OpenOrCreate, FileAccess.Write);

// Endre standardfeilstrømmen til å peke på filstrømmen
Console.SetError(errorStream);

// Skriv en feilmelding til standardfeil
Console.Error.WriteLine("Dette er en feilmelding som blir skrevet til error.log");

// Husk å lukke filstrømmen når du er ferdig
errorStream.Close();
```

Svært ofte vil du ønske å skrive til både standardutgang og standardfeil. Dette kan enkelt gjøres ved å bruke Console.WriteLine() og Console.Error.WriteLine() i kombinasjon. Da vil både standardutgang og standardfeilstrømmen bli skrevet til, men standardfeil vil bli skrevet i rødt for å skille det fra standardutgangen.

## Dypdykk

Det er viktig å merke seg at feil som oppstår under kjøring av et program automatisk blir skrevet til standardfeil, så du trenger ikke nødvendigvis å bruke Console.Error.WriteLine() for å få informasjon om feil. Det er bare når du ønsker å gi brukeren spesifikk informasjon om en feil eller unntak i koden din at du vil bruke denne metoden.

Å skrive til standardfeil er også nyttig når du jobber med flertrådede applikasjoner. Hver tråd har sin egen standardutgang og standardfeilstrøm, så ved å skrive til standardfeil kan du få informasjon om eventuelle feil i de individuelle trådene.

## Se Også

- [C# Console Error] (https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [C# Console SetError()] (https://docs.microsoft.com/en-us/dotnet/api/system.console.seterror)
- [Debugging in C#] (https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)