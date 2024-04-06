---
date: 2024-01-20 17:47:22.666813-07:00
description: "Hvordan gj\xF8re det: Output."
lastmod: '2024-04-05T21:53:41.762146-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Finn lengden p\xE5 en streng"
weight: 7
---

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
