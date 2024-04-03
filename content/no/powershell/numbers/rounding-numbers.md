---
date: 2024-01-26 03:46:24.910441-07:00
description: "Avrunding av tall handler om \xE5 justere en verdi til n\xE6rmeste heltall\
  \ eller angitt desimalplass. Programmerere runder av tall for \xE5 forenkle data,\
  \ forbedre\u2026"
lastmod: '2024-03-13T22:44:41.011168-06:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall handler om \xE5 justere en verdi til n\xE6rmeste heltall\
  \ eller angitt desimalplass."
title: Avrunding av tall
weight: 13
---

## Hvordan:
Du har noen hendige cmdlets og metoder i PowerShell for avrunding:

- `Round()`-metoden fra Math-klassen
```PowerShell
[Math]::Round(15.68) # Runder til 16
```
- Spesifiser desimaler:
```PowerShell
[Math]::Round(15.684, 2) # Runder til 15.68
```
- `Ceiling()` og `Floor()`, for alltid å runde opp eller ned:
```PowerShell
[Math]::Ceiling(15.2) # Runder opp til 16
[Math]::Floor(15.9) # Runder ned til 15
```

## Dypdykk
Avrunding av tall er ikke noe nytt; det har vært med oss siden antikken, nyttig for handel, vitenskap og tidsmåling. Når det gjelder PowerShell, følger `[Math]::Round()` "Bankers Avrunding" som standard, der 0.5 går til det nærmeste partall, noe som reduserer bias i statistiske operasjoner.

Du er ikke bare fast med `[Math]`-metodene, though. Vil du ha mer kontroll? Sjekk ut `[System.Math]::Round(Number, Digits, MidpointRounding)` hvor du kan sette hvordan midtpunkter håndteres: vekk fra null eller til partall (aka Bankers Avrunding).

Et annet perspektiv: `System.Globalization.CultureInfo`-objektet. Det bidrar med lokalspesifikk formatering og avrundingspreferanser når du håndterer internasjonale tall.

## Se også
- Microsofts offisielle dokumenter om Math-metoder: [Lenke](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- Spesifikasjoner for avrunding av desimaler i .NET: [Lenke](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- Diskusjoner om avrunding i StackOverflow: [Lenke](https://stackoverflow.com/questions/tagged/rounding+powershell)
