---
date: 2024-01-20 17:55:50.198939-07:00
description: "How to: K\xE4ynnist\xE4 ohjelma komennolla ja argumenteilla, esim. `myapp.exe\
  \ -mode console -verbose`. C#:ssa argumentit k\xE4sitell\xE4\xE4n `Main`-metodissa."
lastmod: '2024-03-13T22:44:56.589362-06:00'
model: gpt-4-1106-preview
summary: "K\xE4ynnist\xE4 ohjelma komennolla ja argumenteilla, esim."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
Käynnistä ohjelma komennolla ja argumenteilla, esim. `myapp.exe -mode console -verbose`. C#:ssa argumentit käsitellään `Main`-metodissa:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        foreach(var arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Kutsuttaessa ohjelmaa komennolla `myapp.exe first second`, tulostuu:

```
first
second
```

## Deep Dive
Komentoriviparametrien lukeminen on vanhaa perua jo 1960-luvulta saakka, kun käyttöliittymät olivat tekstipohjaisia. C#:ssa `Main`-metodin `args`-taulukko on standardi tapa, mutta on olemassa myös nykyaikaisempia keinoja, kuten `System.CommandLine`-kirjasto, mikä tuo voimakkaampia työkaluja argumenttien hallintaan. Perinteisessä `args`-taulukossa argumentit ovat vain merkkijonoja, joten niitä täytyy käsin parsia ja tyyppimuuntaa kunnolliseen käyttöön, toisin kuin `System.CommandLine`-kirjaston kanssa.

## See Also
- Microsoftin C#-dokumentaatio komennoriviparametreista: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/
- System.CommandLine GitHub-sivusto: https://github.com/dotnet/command-line-api
- Stack Overflow keskustelut ja esimerkit: https://stackoverflow.com/questions/tagged/command-line-arguments?tab=Votes
