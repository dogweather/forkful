---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk er et kraftig verktøy som lar programmerere filtrere, manipulere og matche tekst. Vi bruker det fordi det forenkler og effektiviserer tekstbehandling, noe som er vanlig i mange programmeringsoppgaver. 

## Hvordan gjøre det:
Her er en forenklet måte å bruke regulære uttrykk i C#. 

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testString = "Hei, mitt navn er Ola og min epost er ola@gmail.com";
        Regex regex = new Regex(@"(\b[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,7}\b)");
        
        Match match = regex.Match(testString);

        if(match.Success)
        {
            Console.WriteLine("Funnet e-post: " + match.Value);
        }
    }
}
```
Outputt blir :
```
Funnet e-post: ola@gmail.com
```

## Dypdykk
Regulære uttrykk har sin opprinnelse fra teoretisk informatikk, introdusert av matematiker Stephen Cole Kleene på 1950-tallet. Alternativene til regulære uttrykk inkluderer tekstbehandlingsbiblioteker og innebygde funksjoner i programmeringsspråk. Men regulære uttrykk har en universalitet som overgår de fleste av disse alternativene.

Bruk av regulære uttrykk i C# er ordnet med `System.Text.RegularExpressions` biblioteket. Det lar deg kompilere og bruke regulære uttrykk på tekststrenger. En viktig detalj er at du må være forsiktig med spesielle tegn, som '\\'. Disse må dobbelteskapas i regulære uttrykk i C#, for eksempel '\\\\d' for å matche et hvilket som helst siffer.

## Se ogsa
1. Offisielt .NET dokuments om Regex: [Her](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8)
2. TutorialsPoint guide til regulære uttrykk i C#: [Her](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)
3. Regexr, et nyttig verktøy for testing av regulære uttrykk: [Her](https://regexr.com/)