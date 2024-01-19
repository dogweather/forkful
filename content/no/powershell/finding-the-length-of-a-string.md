---
title:                "Finne lengden på en streng"
html_title:           "Go: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Lær PowerShell: Hvordan finne lengden på en streng

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall tegn den inneholder. Det er praktisk for programmerere for å håndtere brukerinngang, validere data, eller manipulere tekster.

## Hvordan:
Her er et enkelt eksempel på hvordan du kan finne strenglengde i PowerShell:

```PowerShell
$streng = "Hallo Verden!"
$length = $streng.Length;
Write-Output "Strenglengden er: $length"
```

Resultatet vil se slik ut:

```
Strenglengden er: 13
```

## Dyp Dykk

1. Historisk kontekst: Selv om strenglengdefunksjonen er grunnleggende, kommer den ikke alltid innebygget. PowerShell, som er basert på .NET, gjorde dette til gjenstand for egenskapen `.Length` i stedet for en funksjon.

2. Alternativer: Du kan også bruke `Measure-Object` cmdlet som en alternativ metode:

   ```PowerShell
   "Hallo Verden!" | Measure-Object -Character | Select-Object -Property Characters
   ```
   
3. Implementasjonsdetaljer: `.Length` og `Measure-Object` opererer forskjellig. Mens `Length` er en egenskap fra System.String klasse, teller `Measure-Object` tegnene gjennom en pipeline. `Length` har en bedre ytelse, men `Measure-Object` gir mer detaljert informasjon.

## Se Også

For videre lesing om bruk av strenger i PowerShell, sjekk ut disse kildene:

- [Microsofts veileder om strenger](https://docs.microsoft.com/nb-no/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- [PowerShell strengmanipulasjon](https://adamtheautomator.com/powershell-string-manipulation/)
- [Strenglengde i PowerShell - Stack Overflow](https://stackoverflow.com/questions/1122555/get-string-length-in-powershell)