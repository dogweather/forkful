---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulære uttrykk er mønstersøk i tekst. Programmerere bruker dem for effektivt å søke, validere eller manipulere strenger.

## How to:
```c#
using System;
using System.Text.RegularExpressions;

class RegularExpressionsDemo
{
    static void Main()
    {
        string pattern = @"\d+"; // Enkelt mønster for å finne alle tall
        string text = "Det er 5 epler i 3 kurver.";

        MatchCollection matches = Regex.Matches(text, pattern);

        foreach (Match match in matches)
        {
            Console.WriteLine($"Fant tall: {match.Value}");
        }

        // Output:
        // Fant tall: 5
        // Fant tall: 3
    }
}
```

## Deep Dive
Regulære uttrykk stammer fra teoretisk informatikk, utviklet på 1950-tallet. Alternativer til regulære uttrykk inkluderer string-søkefunksjoner (som `IndexOf` eller `Contains` i .NET) når enkelt søk er nødvendig, og parser-biblioteker for kompleks tekstbehandling. Implementasjonsdetaljer i .NET bruker backtracking for fleksibilitet og kompleksitetskontroll.

## See Also
- Microsoft's Regular Expressions Documentation: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- RegexOne - læringsguide for regulære uttrykk: https://regexone.com/
- Stack Overflow - praktiske spørsmål og svar: https://stackoverflow.com/questions/tagged/regex
