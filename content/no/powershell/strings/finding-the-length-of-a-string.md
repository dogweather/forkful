---
date: 2024-01-20 17:47:51.987661-07:00
description: "\xC5 finne lengden av en streng betyr \xE5 telle antall tegn i den.\
  \ Programmerere gj\xF8r dette for validering, l\xF8kkekontroll og tekstmanipulering."
lastmod: '2024-03-13T22:44:41.007390-06:00'
model: gpt-4-1106-preview
summary: "\xC5 finne lengden av en streng betyr \xE5 telle antall tegn i den. Programmerere\
  \ gj\xF8r dette for validering, l\xF8kkekontroll og tekstmanipulering."
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## What & Why?
Å finne lengden av en streng betyr å telle antall tegn i den. Programmerere gjør dette for validering, løkkekontroll og tekstmanipulering.

## How to:
Her er noen enkle måter å finne lengden på en streng i PowerShell:

```PowerShell
$streng = "Hei, Norge!"
$length = $streng.Length
Write-Output $length  # Gir ut 11
```

Du kan også bruke `Measure-Object` cmdlet:

```PowerShell
$streng = "Hei, Norge!"
$length = ($streng | Measure-Object -Character).Characters
Write-Output $length  # Gir ut 11
```

## Deep Dive
Før PowerShell var det vanligvis nødvendig å skrive mer kompleks kode i eldre språk som C eller VBScript for å oppnå lignende funksjonalitet. Med innføringen av `.Length`-egenskapen i .NET, som PowerShell bygger på, ble oppgaven enkel og direkte.

Det finnes alternative måter å måle strenglengde på i andre programmeringsspråk, som `strlen()` i PHP eller `len()` i Python, men i PowerShell er `.Length` og `Measure-Object` de vanligste.

Teknisk sett er `.Length` en egenskap definert på System.String-objekter i .NET Framework, som returnerer antall Char-objekter i denne strengen. Den teller faktiske tegn, inkludert mellomrom og skjulte tegn.

## See Also
- Microsofts dokumentasjon på `Measure-Object`: [Measure-Object](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/measure-object?view=powershell-7.1)
- StackOverflow diskusjoner om strengmanipulering i PowerShell: [PowerShell - String Length](https://stackoverflow.com/questions/tagged/powershell+string+length)
