---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Å skrive ut feilsøkingsutdata refererer til å vise mellomliggende informasjon fremfor kun det endelige resultatet. Programmere gjør dette for bedre å forstå hva som skjer i koden deres, og å fikse eventuelle feil.

## Hvordan: 

Du kan enkelt skrive ut feilsøkingsinfo i PowerShell ved å bruke `Write-Debug`, slik:

```PowerShell
function Test-Debug {
    param()
    Write-Debug "Inside the function"
}
```

Kjør funksjonen med DebugPreference satt til 'Continue':

```PowerShell
$DebugPreference = 'Continue'
Test-Debug
```

Output:

```PowerShell
DEBUG: Inside the function
```

## Dypdykk:

1. Historisk har innledende debugmetoder variert fra simpel kodesjekking til fysiske lysdioder. PowerShell forenkler debugging ved å implementere innebygde kommandoer.

2. Alternativt til `Write-Debug`, kan PowerShell også bruke `Write-Verbose` for mer detaljert feilsøkingsinfo.

```PowerShell
function Test-Verbose {
    param()
    Write-Verbose "Inside the function" -Verbose
}

Test-Verbose
```

Output:

```PowerShell
VERBOSE: Inside the function
```

3. PowerShell bruker et system med preferanser for å kontrollere hvordan feilsøking opptre. Disse innstillingene kan angis individuelt og overstyres ved behov.

## Se Også:

1. Offisiell PowerShell dokumentasjon: [About PREFERENCE Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables?view=powershell-7)
2. Velgjørende Bloggartikkel: [Debugging with PowerShell](https://devblogs.microsoft.com/scripting/debugging-with-powershell/)