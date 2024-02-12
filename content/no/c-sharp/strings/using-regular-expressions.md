---
title:                "Bruke regulære uttrykk"
aliases:
- /no/c-sharp/using-regular-expressions/
date:                  2024-02-03T19:16:31.690021-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke regulære uttrykk"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk (regex) i C# er et kraftig verktøy for mønstersøking i strenger, som lar programmerere søke etter, erstatte, splitte eller trekke ut data effektivt. Programmerere bruker regex for oppgaver som strekker seg fra enkle valideringer, som sjekking av e-postformat, til komplekse tekstbehandlingsoppgaver på grunn av dets fleksibilitet og ytelse.

## Hvordan:

### Enkel Mønstersøking
For å sjekke om en streng inneholder et spesifikt mønster, kan du bruke `Regex.IsMatch`-metoden fra `System.Text.RegularExpressions` navneområdet.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Utdata: True
    }
}
```

### Trekke Ut Data
Å trekke ut data fra en streng ved bruk av grupper i et regex kan gjøres med `Regex.Match`-metoden.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Dato: 2023-04-12";
        string pattern = @"Dato: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"År: {match.Groups[1].Value}");  // Utdata: År: 2023
            Console.WriteLine($"Måned: {match.Groups[2].Value}");  // Utdata: Måned: 04
            Console.WriteLine($"Dag: {match.Groups[3].Value}");  // Utdata: Dag: 12
        }
    }
}
```

### Erstatte Tekst
`Regex.Replace`-metoden lar deg erstatte tekst i en streng som matcher et spesifisert mønster.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Besøk Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Utdata: Besøk Google!
    }
}
```

### Splitte Strenger
Du kan splitte en streng i et array basert på et regex-mønster ved bruk av `Regex.Split`-metoden.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "en,to,tre,fire,fem";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Utdata: 
        // en
        // to
        // tre
        // fire
        // fem
    }
}
```

### Bruke Tredjepartsbiblioteker
Selv om .NET-rammeverket gir omfattende støtte for regulære uttrykk, finnes det også tredjepartsbiblioteker som `PCRE.NET` som tilbyr Perl-kompatible regulære uttrykk (PCRE) i C#. Dette kan være nyttig hvis du trenger funksjoner eller syntaks fra Perls regex-motor som ikke er tilgjengelig i .NETs implementering.

For å bruke `PCRE.NET`, må du først installere dets NuGet-pakke, og deretter kan du bruke det på lignende måte som du bruker de native .NET regex-klassene.

```csharp
// Eksempel ved bruk av PCRE.NET her
// Merk: Forestill deg et eksempel lignende de over, skreddersydd for å vise en funksjon unik for PCRE.NET.
```

Når du integrerer tredjepartsbiblioteker for regulære uttrykk, bør du alltid konsultere deres dokumentasjon for detaljert bruk og kompatibilitetsinformasjon.
