---
date: 2024-01-26 03:41:15.660312-07:00
description: "Hur: Du kan anv\xE4nda `-replace`-operatorn f\xF6r att ta bort citationstecken\
  \ fr\xE5n en str\xE4ng. S\xE5 h\xE4r g\xF6r du."
lastmod: '2024-03-13T22:44:38.112136-06:00'
model: gpt-4-0125-preview
summary: "Du kan anv\xE4nda `-replace`-operatorn f\xF6r att ta bort citationstecken\
  \ fr\xE5n en str\xE4ng."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur:
Du kan använda `-replace`-operatorn för att ta bort citationstecken från en sträng. Så här gör du:

```PowerShell
# Ersätt enkla citationstecken
$stringWithSingleQuotes = "'Hej, världen!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Utmatning: Hej, världen!

# Ersätt dubbla citationstecken
$stringWithDoubleQuotes = '"Hej, världen!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Utmatning: Hej, världen!
```

För båda typerna:

```PowerShell
$stringWithQuotes = '"Hejsan," sa hon.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Observera användningen av regex teckenklass
Write-Output $cleanString  # Utmatning: Hejsan, sa hon.
```

Ett exempel på utmatning från konsolen kommer se ut något så här:

```
Hej, världen!
Hej, världen!
Hejsan, sa hon.
```

## Djupdykning
Tillbaka i tiden, innan PowerShell var en glimt i Microsofts öga, var textbearbetning i Windows ofta domänen för batch-skript som hade begränsade möjligheter. PowerShell introduktion medförde kraftfulla strängmanipuleringsfunktioner som gjorde skriptning mycket robustare.

Det finns alternativ till `-replace`, som att använda `.Trim()`-metoden för att ta bort citationstecken enbart i början och slutet av en sträng, men de erbjuder inte samma kontroll eller regex-stöd.

```PowerShell
# Använder .Trim() för citationstecken i början och slutet
$stringWithQuotes = '"Hej, världen!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Utmatning: Hej, världen!
```

Notera, `-replace` använder regex bakom kulisserna, så när du arbetar med det, kom ihåg att speciella tecken behöver undantas om du riktar in dig på dem. Om du behöver mer granulär kontroll över borttagningen av citationstecken är det att dyka in i regex med `-replace` sättet att gå, vilket ger dig enorm flexibilitet.

## Se också
- För mer om regex i PowerShell, se de officiella dokumenten: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Upptäck andra strängmetoder: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
