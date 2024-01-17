---
title:                "Sammenligne to datoer"
html_title:           "PowerShell: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sammenligne to datoer er en vanlig oppgave for programmerere. Dette innebærer å sjekke om to datoer er like eller om en dato kommer før eller etter en annen. Dette er viktig for å sørge for at programmeret fungerer riktig og behandler datoer på en konsistent måte.

## Hvordan:

```PowerShell
$firstDate = Get-Date "2021-01-01"
$secondDate = Get-Date "2021-01-05"

# Sjekker om første dato er lik andre dato
if ($firstDate -eq $secondDate) {
  Write-Host "Datoene er like"
}

# Sjekker om første dato er før andre dato
if ($firstDate -lt $secondDate) {
  Write-Host "Første dato kommer før andre dato"
}

# Sjekker om første dato er etter andre dato
if ($firstDate -gt $secondDate) {
  Write-Host "Første dato kommer etter andre dato"
}
```

Output:
```
Første dato kommer før andre dato
```

## Dypdykk:

Å sammenligne datoer har vært en utfordring for programmerere i lang tid, spesielt når man tar hensyn til ulike datoformater og tidsstyringssystemer. Alternativene til å bruke innebygde funksjoner som ```-eq```, ```-lt``` og ```-gt``` inkluderer å bruke .NET-metoder eller å konvertere datoene til tall og sammenligne dem. Implementeringen av disse alternativene kan være mer kompleks og mindre effektive enn å bruke de innebygde funksjonene i PowerShell.

## Se også:

For mer informasjon om å håndtere datoer i PowerShell, sjekk ut disse ressursene:

- [Working with Dates and Times in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Comparison Operators in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)