---
title:    "C#: Å finne lengden på en streng"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng kan virke som en enkel oppgave, men det er en viktig ferdighet i enhver programmerers verktøykasse. Å vite lengden på en streng kan være nyttig når du jobber med tekstmanipulasjon eller når du trenger å kontrollere inndata som er begrenset i størrelse. I denne bloggposten vil vi utforske hvordan du kan finne lengden på en streng i C#.

## Hvordan

For å finne lengden på en streng i C#, kan du bruke den innebygde metoden `Length`. La oss se på et eksempel:

```C#
string navn = "Ola";
Console.WriteLine(navn.Length);
```

Dette vil skrive ut `3` siden `navn` er en streng bestående av tre tegn. Hvis strengen derimot er tom, vil `Length` returnere `0`.

Det er også verdt å nevne at `Length` returnerer antall tegn, ikke antall ord, i en streng. Hvis du ønsker å finne antall ord, må du først splitte strengen i et array av ord og deretter finne lengden på arrayet.

```C#
string tekst = "Dette er en lang setning.";
string[] ord = tekst.Split(" ");
Console.WriteLine(ord.Length);
```

Dette vil skrive ut `5` siden det er 5 ord i setningen.

## Dypdykk

En av de mest brukte metodene for å finne lengden på en streng er å bruke løkken `for`. La oss se på et eksempel:

```C#
string streng = "Hei, dette er en streng.";
int lengde = 0;

for (int i = 0; i < streng.Length; i++)
{
    lengde++;
}

Console.WriteLine(lengde);
```

Denne løkken vil sammenligne hver bokstav i strengen og øke lengden med 1 for hver bokstav. Til slutt vil lengden bli skrevet ut, som i dette tilfellet vil være `24` siden det er 24 tegn i strengen.

Husk at dette bare er ett av flere mulige tilnærminger for å finne lengden på en streng. Det finnes også andre metoder og innebygde funksjoner som kan brukes for dette formålet.

## Se Også

- [C# String Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [C# for Beginners: Working with Strings](https://www.educative.io/edpresso/c-for-beginners-working-with-strings)
- [How to Measure the Length of a String in C#](https://www.c-sharpcorner.com/UploadFile/fd0172/how-to-find-the-length-of-a-string-in-C-Sharp/)