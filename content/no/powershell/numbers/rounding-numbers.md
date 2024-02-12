---
title:                "Avrunding av tall"
aliases:
- no/powershell/rounding-numbers.md
date:                  2024-01-26T03:46:24.910441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Avrunding av tall handler om å justere en verdi til nærmeste heltall eller angitt desimalplass. Programmerere runder av tall for å forenkle data, forbedre lesbarheten, eller møte visse matematiske krav under beregninger.

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
