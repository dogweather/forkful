---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla första tecknet i ordet till versal (stort bokstav), detta kan vara viktigt för att standardisera data eller för presentationssyften.

## Hur man gör:
Använd `.ToUpper()` för att göra hela strängen versal eller `.ToLower()` följt av `.Substring()` och `+` operatorn för att kapitalisera bara det första tecknet.

```PowerShell
$text = "välkommen till PowerShell"
$text.ToUpper() # Gör hela strängen versal
$text.Substring(0,1).ToUpper() + $text.Substring(1) # Gör första tecknet versalt
```
Sample Output:
```
VÄLKOMMEN TILL POWERSHELL
Välkommen till PowerShell
```

## Fördjupning
Historiskt sett användes stora bokstäver i början av meningar för att förbättra läsbarheten. I PowerShell, precis som i många andra programmeringsspråk, kan strängmanipulation vara en del av programmets logik för att till exempel ordna data eller ändra utseendet i användargränssnitt.

Det finns alternativ till att använda `.Substring()` som att använda `-creplace '^.', {$_.Value.ToUpper()}` för att kapitalisera första tecknet med regular expressions.

```PowerShell
$text = "välkommen"
$text -creplace '^.', {$_.Value.ToUpper()}
```

I omständigheter där prestanda är kritisk, är det värt att komma ihåg att konkatenering med '+' kan vara mindre effektivt än att använda en StringBuilder-klass i .NET om det handlar om stora eller många strängmanipulationer.

## Se också
- Regular expression stöd i PowerShell: [about_Regular_Expressions](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- En guide till .NET:s StringBuilder-klass: [StringBuilder Class](https://docs.microsoft.com/dotnet/api/system.text.stringbuilder)
