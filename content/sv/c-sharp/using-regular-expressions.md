---
title:                "Använda reguljära uttryck"
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck är mönster för att leta efter specifika teckensträngar i en text. Programmerare använder dem för att validera, parse:a och manipulera text på ett kraftfullt och flexibelt sätt.

## How to: Så här gör du

För att använda reguljära uttryck i C#, inkludera `using System.Text.RegularExpressions;` och använd `Regex` klassen. Här är ett exempel som hittar alla e-postadresser i en text:

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string text = "Kontakta oss på info@example.com eller support@example.org.";
        string pattern = @"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b";

        MatchCollection matches = Regex.Matches(text, pattern);

        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```

Output:
```
info@example.com
support@example.org
```

## Deep Dive: Djupdykning

Regular expressions, ofta förkortat regex, dök upp på 1950-talet. I C# hanteras de genom `Regex` klassen i `System.Text.RegularExpressions` namnrymden. En del alternativ till regex inkluderar string metoder som `Contains`, `IndexOf` och `Substring` men dessa saknar regex flexibilitet och kraft. `Regex` klassen använder en intern lagningsalgoritm som kan optimeras med `RegexOptions.Compiled` för förbättrad prestanda vid upprepade anrop.

## See Also: Se även

- Microsofts dokumentation om reguljära uttryck i .NET: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- Regex101, ett online verktyg för att testa regex mönster: [regex101.com](https://regex101.com/)
- En guide till .NET reguljära uttryck prestanda: [blog.codinghorror.com](https://blog.codinghorror.com/to-compile-or-not-to-compile/)