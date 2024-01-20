---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generer tilfeldige tall i C# 

## Hva og hvorfor?

Å generere tilfeldige tall betyr at man lager et tall som ikke kan forutsies rasjonalet, i en viss definert rekkevidde. Dette er viktig for mange aspekter i programmering, slike som spill, sikkerhet, håndtering av data, og mye mer.

## Slik gjør du:

Her er et enkelt eksempel på hvordan du kan generere tilfeldige tall i C#.

```C#
using System;

class Program
{
    static void Main()
    {
        Random rand = new Random();
        int tilfeldigTall = rand.Next(1, 100); // generer et tall mellom 1 og 100
        Console.WriteLine(tilfeldigTall);
    }
}
```

Når du kjører dette programmet, vil det skrive ut et tilfeldig tall mellom 1 og 100 hver gang.


## Dypdykk

I tidligere versjoner av programmeringsspråk, var det mer komplisert å generere tilfeldige tall, og man måtte ofte bruke algoritmer som f.eks. Middle-square metoden. 

I C# har vi klassen `Random` som brukes for å generere tilfeldige tall. Den genererer et pseudo-tilfeldige tall, det vil si at tallene ser tilfeldige ut, men vil følge samme rekkefølge hvis du bruker samme startverdi (eller seed).

Alternativt, hvis du trenger sikkerhetssensitive tilfeldige tall (som for eksempel ved generering av kryptografiske nøkler), bør du bruke klassen `RNGCryptoServiceProvider` istedenfor. Dette er en klasse som er laget for å generere tilfeldige tall som er sterke nok for kryptografi.

## Se også

For mer informasjon om generering av tilfeldige tall, kan du se på følgende ressurser:
* [.NET API Dokumentasjon - Random](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
* [.NET API Dokumentasjon - RNGCryptoServiceProvider](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)
* [How To Generate Random Numbers In C#](https://www.c-sharpcorner.com/article/how-to-generate-random-numbers-in-C-Sharp/)