---
title:                "Generering av tilfeldige tall"
html_title:           "PowerShell: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å generere tilfeldige tall er en vanlig oppgave for programmører, enten det er snakk om å lage et spill eller for testing og simulering. Å ha tilfeldige tall er viktig for å gjøre programmene mer varierte og realistiske.

# Hvordan?
Bruk `Get-Random`-kommandoen for å generere et enkelt tilfeldig tall:
```PowerShell
Get-Random
```
Dette vil gi deg et tilfeldig tall mellom 0 og `Int32.MaxValue` (2,147,483,647). Hvis du ønsker å begrense det til et spesifikt område, kan du bruke `-Minimum` og `-Maximum` parametrene:
```PowerShell
Get-Random -Minimum 1 -Maximum 10
```
Dette vil gi deg et tilfeldig tall mellom 1 og 10. Du kan også angi antall tall du vil ha som output ved å bruke `-Count`-parameteren:
```PowerShell
Get-Random -Minimum 1 -Maximum 10 -Count 5
```
Dette vil gi deg fem tilfeldige tall mellom 1 og 10.

# Dykk dypere
Generering av tilfeldige tall har vært et problem for programmer i lang tid. Tidligere måtte programmerere bruke komplekse algoritmer for å generere tilfeldige tall, men med utviklingen av datamaskiner og programmeringsspråk har vi nå innebygde funksjoner som gjør denne oppgaven mye enklere. 

Alternativt kan du bruke `New-Guid`-kommandoen for å generere et unikt og tilfeldig alfanumerisk ID:
```PowerShell
New-Guid
```

Det er viktig å merke seg at tilfeldige tall generert av datamaskiner ikke er 100% tilfeldig, men basert på en algoritme. Dette betyr at de ikke kan brukes i sikkerhetskritiske applikasjoner, som for eksempel kryptografi.

# Se også
- [Microsoft dokumentasjon om `Get-Random`-kommandoen](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Random?view=powershell-7.1)
- [Alternativer for tilfeldige tallgenerering i PowerShell ved hjelp av .NET-metoder](https://ss64.com/ps/syntax-rndnumber.html)
- [Hvordan generere tilfeldige tall i andre programmeringsspråk](https://www.geeksforgeeks.org/generating-random-number-including-integer-and-floating-point-in-python/)