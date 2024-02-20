---
date: 2024-01-20 17:58:37.382514-07:00
description: "S\xF8k og erstatt lar deg finne tekster og bytte dem ut med noe annet.\
  \ Programmeringsgjengen bruker det for \xE5 raskt oppdatere kode, fikse feil, eller\
  \ endre\u2026"
lastmod: 2024-02-19 22:05:00.269358
model: gpt-4-1106-preview
summary: "S\xF8k og erstatt lar deg finne tekster og bytte dem ut med noe annet. Programmeringsgjengen\
  \ bruker det for \xE5 raskt oppdatere kode, fikse feil, eller endre\u2026"
title: "S\xF8king og erstatting av tekst"
---

{{< edit_this_page >}}

## What & Why?
Søk og erstatt lar deg finne tekster og bytte dem ut med noe annet. Programmeringsgjengen bruker det for å raskt oppdatere kode, fikse feil, eller endre data.

## How to:
```PowerShell
# Enkel søk og erstatt i en tekststreng
$tekst = "Hello, world!"
$oppdatertTekst = $tekst -replace "world", "Norway"
$oppdatertTekst
```
Resultat: `Hello, Norway!`

```PowerShell
# Bruke regex for mer komplekse operasjoner
$streng = "Eple 100, Banan 200"
$oppdatertStreng = $streng -replace "\d+", "150"
$oppdatertStreng
```
Resultat: `Eple 150, Banan 150`

```PowerShell
# Erstatter flere ord
$tekst = "Fjell og dal, skog og fjord."
$ordErstatt = @{
    "fjell" = "åser";
    "dal" = "glen";
}
foreach ($ord in $ordErstatt.Keys) {
    $tekst = $tekst -replace $ord, $ordErstatt[$ord]
}
$tekst
```
Resultat: `åser og glen, skog og fjord.`

## Deep Dive
Søk og erstatt har vært en hjørnestein i programmering siden tidlige teksteditorer som vi brukte på 70-tallet. Alternativer som `sed` i Unix/Linux-miljøer tilbyr lignende funksjoner. I PowerShell bruker `-replace` operator regex (regular expressions) som standard, noe som gir kraftige muligheter for tekstmanipulering.

Selve implementasjonen i PowerShell ligger i .NET-rammeverket, der string-klasser gir søk- og erstatningsfunksjoner. Regex-basert erstatning tillater mønstergjenkjenning for dynamisk tekstbehandling, som er super nyttig i skripting for datarensing, logganalyse, og automatisering av kodeendringer.

## See Also
- [about_Regular_Expressions (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.2)
- [about_Comparison_Operators (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.2)
- [String.Replace Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-6.0)
