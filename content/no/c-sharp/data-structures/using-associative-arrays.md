---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:44.325808-07:00
description: "Hvordan: I C# jobber du med assosiative tabeller ved \xE5 bruke `Dictionary<TKey,\
  \ TValue>`-klassen. Her er et raskt eksempel for \xE5 f\xE5 deg i gang."
lastmod: '2024-03-13T22:44:40.788048-06:00'
model: gpt-4-0125-preview
summary: "I C# jobber du med assosiative tabeller ved \xE5 bruke `Dictionary<TKey,\
  \ TValue>`-klassen."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
I C# jobber du med assosiative tabeller ved å bruke `Dictionary<TKey, TValue>`-klassen. Her er et raskt eksempel for å få deg i gang:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Oppretter en ordbok
        Dictionary<string, int> fruktkurv = new Dictionary<string, int>();

        // Legger til nøkkel-verdi par
        fruktkurv.Add("Epler", 5);
        fruktkurv.Add("Appelsiner", 10);

        // Aksesserer en verdi ved hjelp av nøkkelen
        Console.WriteLine("Epler: " + fruktkurv["Epler"]);
        
        // Oppdaterer en verdi
        fruktkurv["Epler"] = 7;
        Console.WriteLine("Oppdaterte Epler: " + fruktkurv["Epler"]);
        
        // Fjerner et nøkkel-verdi par
        fruktkurv.Remove("Appelsiner");

        // Itererer over ordboken
        foreach (var par in fruktkurv)
        {
            Console.WriteLine(par.Key + ": " + par.Value);
        }
    }
}
```
Eksempel på utskrift:
```
Epler: 5
Oppdaterte Epler: 7
Epler: 7
```

Dette eksempelet viser oppretting av en ordbok, tillegging, tilgang, oppdatering og fjerning av elementer, og iterering over den.

## Dypdykk
Konseptet med assosiative tabeller går tilbake til deres bruk i skriptspråk som Perl og PHP, hvor de tilbyr fleksibilitet i håndtering av datakolleksjoner. I C#, er `Dictionary<TKey, TValue>` den de facto implementasjonen, introdusert i .NET Framework 2.0. Det lagrer data i en hashtabell, som sikrer effektive oppslag, tillegg og slettinger.

Det er imidlertid verdt å merke seg at mens ordbøker er utrolig allsidige, kan de ikke alltid være ditt beste valg. For å opprettholde ordnede samlinger, kan du se på `SortedDictionary<TKey, TValue>` eller `SortedList<TKey, TValue>`, som tilbyr sortert rekkefølge på bekostning av langsommere innsettings- og fjerningsoperasjoner. For scenarioer som krever trådsikkerhet, legger `ConcurrentDictionary<TKey, TValue>` til overhead, men sikrer trygg tilgang fra flere tråder uten manuell låsing.

Til syvende og sist avhenger valget av en implementasjon av assosiative tabeller i C# på dine spesifikke behov med hensyn til orden, ytelse, og trådsikkerhet.
