---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:03.299782-07:00
description: "Tekst in strings zoeken en vervangen stelt je in staat gegevens te updaten\
  \ zonder handmatige wijzigingen. Programmeurs hebben dit nodig om\u2026"
lastmod: '2024-03-11T00:14:24.619451-06:00'
model: gpt-4-0125-preview
summary: "Tekst in strings zoeken en vervangen stelt je in staat gegevens te updaten\
  \ zonder handmatige wijzigingen. Programmeurs hebben dit nodig om\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst in strings zoeken en vervangen stelt je in staat gegevens te updaten zonder handmatige wijzigingen. Programmeurs hebben dit nodig om gebruikersinvoer correcties, gegevensformatting of batchupdates efficiënt af te handelen.

## Hoe:
C# maakt tekstmanipulatie vrij eenvoudig. Hieronder, bekijk de `string.Replace` methode om woorden te verwisselen.

```C#
using System;

public class Program
{
    public static void Main()
    {
        string frase = "Hallo, Wereld!";
        string bijgewerkteFrase = frase.Replace("Wereld", "C#");
        
        Console.WriteLine(bijgewerkteFrase); // Uitvoer: Hallo, C#!
    }
}
```

Geen raketwetenschap, toch? Maar stel we willen hoofdlettergevoeligheid negeren of alleen hele woorden vervangen? Regex schiet te hulp:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string frase = "Appels groeien aan bomen. appeltaarten zijn lekker.";
        string patroon = "\\bappel\\b"; // \b is een woordgrens in Regex
        string vervanging = "Sinaasappel";
        
        string bijgewerkteFrase = Regex.Replace(frase, patroon, vervanging, RegexOptions.IgnoreCase);

        Console.WriteLine(bijgewerkteFrase); // Uitvoer: Sinaasappels groeien aan bomen. Sinaasappeltaarten zijn lekker.
    }
}
```

## Diepere Duik
Vroeger was het manipuleren van strings een gedoe. C was alles wat we hadden, en dat betekende omgaan met karakterarrays en handmatige iteraties. C# gaf ons een cadeau: gemakkelijke stringafhandeling.

Als `string.Replace` of `Regex.Replace` niet volstaan, hebben we opties. Voor enorme teksten of complexe patronen, overweeg een aangepaste parser te schrijven of gebruik bibliotheken zoals Antlr.

Regex is krachtig voor patroonmatching maar kan traag zijn. Als prestaties cruciaal zijn en je houdt van de fijne details, meet en vergelijk dan met `StringBuilder` voor massale, iteratieve vervangingen.

## Zie Ook
- Microsoft Docs over [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace)
- .NET's [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) klasse voor meer gesofisticeerde patronen
- Bekijk Antlr voor complexe parsing: [De ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/)
