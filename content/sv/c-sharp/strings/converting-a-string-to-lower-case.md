---
date: 2024-01-20 17:38:17.062612-07:00
description: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla\
  \ bokst\xE4ver i str\xE4ngen till sm\xE5 bokst\xE4ver. Programmerare g\xF6r detta\
  \ f\xF6r att standardisera\u2026"
lastmod: '2024-03-13T22:44:37.900228-06:00'
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla bokst\xE4\
  ver i str\xE4ngen till sm\xE5 bokst\xE4ver. Programmerare g\xF6r detta f\xF6r att\
  \ standardisera\u2026"
title: "Konvertera en str\xE4ng till gemener"
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
