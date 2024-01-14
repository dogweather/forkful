---
title:    "C#: Skriver til standardfeil"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å kunne skrive til standard error når man koder i C#. Det er et nyttig verktøy for å feilsøke og finne feil i koden din, spesielt når du jobber med større og mer komplekse prosjekter.

## Hvordan

For å skrive til standard error i C#, bruker vi Console.Error.WriteLine() kommandoen. Dette lar deg skrive en streng til standard error, som vil bli skrevet ut i konsollen når programmet kjører.

```C#
Console.Error.WriteLine("Dette er en feilmelding");
```

Dette er spesielt nyttig når du ønsker å skrive ut informasjon om feil som oppstår i koden din, for eksempel når en catch-klausul i en try-catch blokk blir utført.

```C#
try
{
    // Noe kode som kan føre til en feil
}
catch (Exception ex)
{
    Console.Error.WriteLine("En feil oppstod: " + ex.Message);
}
```

Dette vil gjøre det enklere å finne og rette feil i koden din, siden du får en direkte melding om hva som gikk galt.

## Dypdykk

Det er også mulig å lese og behandle standard error ved hjelp av Stream og StreamReader klassene i C#. Dette kan være nyttig når du vil logge feilmeldinger til en fil, i stedet for å skrive dem ut i konsollen.

```C#
Stream standardError = Console.OpenStandardError();
StreamReader reader = new StreamReader(standardError);
string errorLine = reader.ReadLine();

// Gjør noe med feilmeldingen her
```

Du kan også bruke Console.SetError() for å omdirigere standard error til en annen output stream, for eksempel en fil eller en tekstboks i et GUI.

## Se også

- [MSDN - Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [MSDN - Console.SetError Method](https://docs.microsoft.com/en-us/dotnet/api/system.console.seterror)
- [MSDN - FileStream Constructor](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream.-ctor)