---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Fjerning av tegn som matcher et mønster er en grunnleggende strengoperasjon i programmering. Vi gjør det for å manipulere tekster og strenger effektivt, for eksempel for å fjerne uønskede spesialtegn.

## Hvordan:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testTekst = "Hei, [på] deg! Jeg programmerer i C#.";
        Console.WriteLine("Før: " + testTekst);
        string mønster = @"[\[\]]";
        Regex rgx = new Regex(mønster);
        testTekst = rgx.Replace(testTekst, "");
        Console.WriteLine("Etter: " + testTekst);
    }
}
```

Når du kjører dette programmet, får du følgende utdata:

```C#
Før: Hei, [på] deg! Jeg programmerer i C#.
Etter: Hei, på deg! Jeg programmerer i C#.
```

## Dypdykk

Fjerning av tegn som matcher et mønster har vært en del av programmeringsspråk siden de tidlige dagene. C# gjør denne operasjonen enkel og intuitiv ved hjelp av Regexp-klassen.

En annen måte å fjerne tegn som matcher et mønster på, er ved hjelp av LINQ-spørringer, men Regexp-metoden er ofte raskere og mer effektiv.

Fjerning av tegn som matcher et mønster er en strengoperasjon som ofte brukes, spesielt når du arbeider med brukerinngang, logger og noen ganger databaser hvor du kanskje ønsker å fjerne uønskede tegn.

## Se Også

1. [Regex.Replace Method (System.Text.RegularExpressions)](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
2. [C# Regular Expressions](https://www.tutorialsteacher.com/csharp/csharp-regex) 
3. [C# String Manipulation: How to Use Strings in C#](https://stackify.com/csharp-string-manipulation/)