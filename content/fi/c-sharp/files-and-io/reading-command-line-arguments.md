---
title:                "Komennoriviparametrien lukeminen"
aliases: - /fi/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:50.198939-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennoriviparametrien lukeminen on prosessi, jossa ohjelma ottaa syötteenä argumentteja, jotka on annettu ohjelman käynnistyksen yhteydessä. Syy miksi devaajat tekevät tämän? Joustavuuden ja konfiguroinnin mahdollistaminen ilman, että koodia tarvitsee muokata.

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
