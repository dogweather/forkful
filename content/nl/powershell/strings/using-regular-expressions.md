---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:48.711581-07:00
description: "Hoe: Regex is sinds de jaren '50 een integraal onderdeel van programmeren.\
  \ Hoewel PowerShell ingebouwde cmdlets heeft zoals `-match`, `-replace`, en\u2026"
lastmod: '2024-04-05T21:53:51.032608-06:00'
model: gpt-4-0125-preview
summary: Regex is sinds de jaren '50 een integraal onderdeel van programmeren.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe:
```PowerShell
# Een patroon overeenkomen dat begint met 'S', gevolgd door willekeurige karakters, eindigend op 'e'
$patroon = 'S.*e'
$tekst = 'Voorbeeldzin in PowerShell.'
if ($tekst -match $patroon) {
    "Overeenkomst gevonden: $($matches[0])"
}

# Vervang alle voorkomens van 'hond' met 'kat'
$dierenVerhaal = 'De snelle bruine hond springt over de luie hond.'
$dierenVerhaal -replace 'hond', 'kat'
```
Output:
```
Overeenkomst gevonden: Voorbeeldzin in
De snelle bruine kat springt over de luie kat.
```

## Diepere Duik
Regex is sinds de jaren '50 een integraal onderdeel van programmeren. Hoewel PowerShell ingebouwde cmdlets heeft zoals `-match`, `-replace`, en `Select-String` voor regex, bestaan er alternatieven voor tekstmanipulatie – denk aan `string.Contains` of `string.Replace`. PowerShell's regex gebruikt de implementatie van het .NET-framework, waardoor het robuust en rijk aan functies is.

## Zie Ook
- [Microsoft's officiële regex referentie](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [Regex101: Bouw en test regex](https://regex101.com/)
