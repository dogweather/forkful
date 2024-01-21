---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:38:17.062612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Att konvertera en sträng till gemener innebär att omvandla alla bokstäver i strängen till små bokstäver. Programmerare gör detta för att standardisera textdata, vilket underlättar jämförelser och sökningar.

## How to:
I C# konverterar du en sträng till gemener med `ToLower()` metoden. Här är ett kort exempel:

```C#
string example = "Hej Världen!";
string lowerCaseExample = example.ToLower();
Console.WriteLine(lowerCaseExample);
```

Output:

```
hej världen!
```

## Deep Dive
Strängar i C# är objekt av `System.String` klassen. Genom åren har `.ToLower()` metoden varit del av .NET Framework, och senare in .NET Core och .NET 5/6+ som arbetat med Unicode och kulturella aspekter. 

Det finns också `.ToLowerInvariant()`, som ignorerar kulturspecifika regler och använder en opartisk kulturinställning – användbar när man bearbetar data som ska vara konsekvent över olika kulturer.

Hur fungerar det då? `ToLower()` använder kulturella inställningar från `CultureInfo` objektet associerat med den köra tråden. Det betyder att beteendet kan variera beroende på klientens inställningar. Så använd `.ToLowerInvariant()` när du behöver konsistens, oavsett användarens kultur.

## See Also
- MSDN dokumentation om `String.ToLower()`: [https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Information om `CultureInfo` klassen: [https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- Guide om strängjämförelser och CultureInfo: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)