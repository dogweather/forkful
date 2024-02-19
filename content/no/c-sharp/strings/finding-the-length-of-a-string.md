---
aliases:
- /no/c-sharp/finding-the-length-of-a-string/
date: 2024-01-20 17:47:22.666813-07:00
description: "\xC5 finne lengden p\xE5 en streng inneb\xE6rer \xE5 telle antall tegn\
  \ den inneholder. Programmerere gj\xF8r dette for \xE5 validere inndata, iterere\
  \ gjennom tegn, eller\u2026"
lastmod: 2024-02-18 23:08:53.885040
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en streng inneb\xE6rer \xE5 telle antall tegn den\
  \ inneholder. Programmerere gj\xF8r dette for \xE5 validere inndata, iterere gjennom\
  \ tegn, eller\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng innebærer å telle antall tegn den inneholder. Programmerere gjør dette for å validere inndata, iterere gjennom tegn, eller rett og slett for å håndtere tekst på en fornuftig måte.

## Hvordan gjøre det:
```C#
using System;

class Program
{
    static void Main()
    {
        string hilsen = "Hei, Norge!";
        int lengde = hilsen.Length;

        Console.WriteLine("Lengden på strengen er {0}.", lengde);
    }
}
```
Output:
```
Lengden på strengen er 11.
```

## Dypdykk
I tidlige programmeringsspråk kunne det være knotete å finne strenglengder, ofte krevde det manuell iterasjon. I C# er `.Length` eiendommen enkel og rett frem. 

Historisk sett hadde visse språk null-terminerte strenger (f.eks. C), hvor lengden ble funnet ved å telle tegn inntil et nulltegn. C# og .NET Framework bruker et String-objekt som inneholder lengdeinformasjonen, som gjør det raskt tilgjengelig.

Et alternativ til `Length` er å bruke LINQ og `Count()` metoden. Men `Length` er raskere siden `Count()` gjør en iterasjon over hver karakter i strengen.
```C#
int lengde = hilsen.Count();
```

Implementasjonsdetaljer; `Length` er egentlig en offentlig, skrivebeskyttet felt i `String` klassen. Dette feltet oppdateres når strengen endres, så lengden reflekterer alltid det faktiske antallet tegn.

## Se også:
- [Microsoft Docs on Strings](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [.NET API Documentation on String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-7.0)
