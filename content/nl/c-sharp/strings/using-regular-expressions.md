---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:47.475254-07:00
description: 'Hoe te: Laten we kijken naar het matchen, vervangen en splitsen van
  strings met regex in C#. **Een telefoonnummer matchen:**.'
lastmod: '2024-03-13T22:44:50.800018-06:00'
model: gpt-4-0125-preview
summary: Laten we kijken naar het matchen, vervangen en splitsen van strings met regex
  in C#.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe te:
Laten we kijken naar het matchen, vervangen en splitsen van strings met regex in C#.

**Een telefoonnummer matchen:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string patroon = @"\b\d{3}[-.]?\d{3}[-.]?\d{4}\b";
        string tekst = "Bel me op 123-456-7890 of 987.654.3210.";
        MatchCollection matches = Regex.Matches(tekst, patroon);

        foreach (Match match in matches)
           Console.WriteLine(match.Value);
    }
}
```

Uitvoer:
```
123-456-7890
987.654.3210
```

**Nieuwe regels vervangen:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string tekst = "Eerste regel.\nTweede regel.\nDerde regel.";
        string patroon = @"\n";
        string vervanging = " ";

        string resultaat = Regex.Replace(tekst, patroon, vervanging);
        Console.WriteLine(resultaat);
    }
}
```

Uitvoer:
```
Eerste regel. Tweede regel. Derde regel.
```

**Een CSV splitsen:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string tekst = "een,twee,drie,vier";
        string patroon = @",";

        string[] deelStrings = Regex.Split(tekst, patroon);
        foreach (string match in deelStrings)
        {
            Console.WriteLine(match);
        }
    }
}
```

Uitvoer:
```
een
twee
drie
vier
```

## Diepgaande duik
Regex bestaat sinds de jaren 50, dankzij wiskundige Stephen Kleene. Alternatieven voor regex omvatten stringmethoden zoals `Contains`, `IndexOf`, `StartsWith`, enz., maar ze zijn minder krachtig voor complexe patronen.

Wat implementatie betreft, de `Regex` klasse in C# bevindt zich in `System.Text.RegularExpressions`. Het maakt gebruik van backtracking-algoritmen voor patroonherkenning. Regex-bewerkingen kunnen kostbaar zijn; gebruik ze zorgvuldig om prestatievermindering te voorkomen.

## Zie ook
- [Microsoft's Regex Documentatie](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex Tester & Debugger](https://regex101.com/)
- [Beheersing van Reguliere Expressies](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) door Jeffrey Friedl. _Opmerking van [Robert](https://shinyu.org/en/about/): zo heb ik Regexes geleerd. Ik had het gevoel dat ik ze echt begreep na het lezen van het boek. En tegenwoordig, gebruik ik de "Regex Tester & Debugger", hierboven vermeld, wanneer ik er een moet debuggen._
