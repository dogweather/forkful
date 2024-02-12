---
title:                "Slette tegn som matcher et mønster"
aliases:
- no/c-sharp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:10.892385-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Slette tegn som matcher et mønster betyr å finne bestemte sekvenser av tegn i en tekststreng og fjerne dem. Programmerere gjør dette for å rense data, forenkle tekstbehandling eller forberede strenger for spesifikke operasjoner, som databasemigreringer eller nettverksoverføringer.

## Hvordan:
La oss se hvordan dette kan gjøres i C#. Her er et lite script som bruker `Regex`-klassen til å slette alle tall i en tekststreng.

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Hei Norge! Det er 17. mai, 2021.";
        string pattern = @"\d+"; // Matcher alle tallsekvenser

        string output = Regex.Replace(input, pattern, "");

        Console.WriteLine(output);  // Skriver ut: "Hei Norge! Det er . mai, ."
    }
}
```

Og et eksempel som fjerner tegn som ikke er bokstaver eller mellomrom:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "C# er gøy! :)";
        string pattern = @"[^a-zA-Z\s]+"; // Matcher alt som ikke er bokstaver eller mellomrom

        string output = Regex.Replace(input, pattern, "");
        
        Console.WriteLine(output);  // Skriver ut: "C# er gøy "
    }
}
```

## Deep Dive
Regex, eller regulære uttrykk, har vært brukt i mange år for tekstmanipulering; de er kraftige og fleksible. I C# finnes de i `System.Text.RegularExpressions`-navneområdet.

Alternativer til `Regex` kan være `string`-metoder som `Replace`, `Remove` eller `IndexOf`. Disse er enklere og raskere, men mindre fleksible. 

For implementasjoner er det verdt å merke seg at `Regex` kan være tregt ved store mengder data eller komplekse mønstre. Kompilering av `Regex` (ved å bruke `RegexOptions.Compiled`) kan forbedre ytelsen i slike tilfeller.

## Se Også
- [Microsofts dokumentasjon om Regex i .NET](https://docs.microsoft.com/dotnet/standard/base-types/regular-expressions)
- [Regex101: for å teste regulære uttrykk online](https://regex101.com/)
- ["Mastering Regular Expressions" av Jeffrey Friedl](http://shop.oreilly.com/product/9780596528126.do) - En bok om dypere forståelse av regulære uttrykk.
- [Stack Overflow: Tips for å forbedre Regex-ytelse i C#](https://stackoverflow.com/questions/tagged/regex+c%23)
