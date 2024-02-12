---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/powershell/printing-debug-output/
date:                  2024-01-20T17:53:23.431143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
"## Hva & Hvorfor?"

Printing debug output i PowerShell er som å legge små lys langs veien: det hjelper deg å se hvor du er og hvor du skal. Vi gjør det for å spore verdiene og flyten i koden, noe som er uvurderlig for feilsøking og å forstå hva programmet faktisk gjør.

## How to:
"## Slik gjør du:"

```PowerShell
# Enkel utskrift
Write-Host "Hei, dette er min debug-melding"

# Utskrift med variabler
$variabel = "verdi"
Write-Debug "Debug: variabelen har verdt '$variabel'"

# Aktiverer debug-utskrift
$DebugPreference = 'Continue'

# Avansert bruk med if-setninger
if ($variabel -eq "feil verdi") {
    Write-Debug "Noe gikk galt med variabelen"
}
```
Sample output (antagelse: `$variabel har verdi "feil verdi"`):
```
DEBUG: Debug: variabelen har verdt 'feil verdi'
DEBUG: Noe gikk galt med variabelen
```

## Deep Dive:
"## Dypdykk":

Print-debugging er like gammel som programmeringen selv. Før fargeskjermer og grafikk, var det konsollen som var kongen. PowerShell tilbyr flere cmdlets for dette formålet, som Write-Host, Write-Output, og Write-Debug. Write-Host maler rett på skjermen og brukes for øyeblikkelig feedback, mens Write-Output sender objekter videre langs pipeline. Write-Debug er nyttig når du kun vil se meldinger når du trenger dem, aktivert ved å sette `$DebugPreference`.

Et alternativ til PowerShell's innebygde cmdlets er å bruke logging rammerverk som PSFramework. Dette gir et kraftig sett av funksjoner for ikke bare å skrive ut debugging informasjon, men også for å styre og formatere det.

I bunn og grunn, ligger mekanismen for output i hvordan PowerShell behandler strømmer. Det er fem hovedtyper: Output, Error, Warning, Verbose og Debug, hver med sitt formål og sin bruk.

## See Also:
"## Se Også":

- Official PowerShell documentation on about_Write-Debug: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug
- About Streams in PowerShell: https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-objects-output-streams
- PSFramework GitHub page for advanced logging capabilities: https://github.com/PSFramework/PSFramework
