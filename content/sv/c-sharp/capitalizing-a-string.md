---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att 'capitaliza' en sträng innebär att göra så att varje bokstav i strängen blir en stor bokstav. Programmerare gör detta för att standardisera data, förbättra användarupplevelsen eller göra text mer synlig.

## Hur gör man:
Kolla kodexemplen nedan för att se hur man förvandlar text till versaler i C#.

```C#
using System;

class CapitalizeString
{
    static void Main()
    {
        string originalText = "hej sverige!";
        string upperText = originalText.ToUpper();

        Console.WriteLine(upperText);  // Output: HEJ SVERIGE!
    }
}
```

Andra verktyg i .NET använder CultureInfo för att specificera kulturella konventioner:

```C#
using System;
using System.Globalization;

class CapitalizeString
{
    static void Main()
    {
        string originalText = "hej sverige!";
        // Specific for Swedish culture
        string upperText = originalText.ToUpper(new CultureInfo("sv-SE"));

        Console.WriteLine(upperText);  // Output: HEJ SVERIGE!
    }
}
```
## Deep Dive
Att göra bokstäver i en sträng till versaler är något som har stötts sedan de tidigaste programmeringsspråken. I C#, `ToUpper()` är metoden som används oftast för detta, och den kommer från .NET Frameworks `System.String` klass. Det är viktigt att tänka på att vissa språk har unika regler för kapitalisering, vilket C#'s `CultureInfo` kan hjälpa till med att hantera.

Om prestanda är en fråga, eller om endast specifika delar av strängen ska vara i versaler, kan andra metoder eller tekniker vara mer lämpliga:

- Använde `StringBuilder` om du ska förändra en sträng många gånger.
- Att använda LINQ för att capitaliza endast vissa bokstäver eller ord.

Men kom ihåg: `ToUpper()` är ditt go-to för enkelhet och direkt användning inbyggd direkt i .NET.

## Se även
För vidare läsning och relaterade ämnen:

- [Microsoft Docs – ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-6.0)
- [Microsoft Docs – CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [Stack Overflow – When to use CultureInfo](https://stackoverflow.com/questions/20978/when-to-use-cultureinfo-currentculture-or-cultureinfo-currentuiculture)
