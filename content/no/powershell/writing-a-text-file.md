---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive en tekstfil i PowerShell betyr å lagre tekstinformasjon til en fil på harddisken. Programmerere gjør dette for å lagre data, logge informasjon, eller å automatisere konfigurasjonsfiler.

## Slik gjør du:
For å skrive til en tekstfil, bruk `Out-File`, `Set-Content` eller `Add-Content` cmdlets.

```PowerShell
# Skriver ny tekst til en fil, overskriver eksisterende innhold
"Hello, World!" | Out-File -FilePath "hilsen.txt"

# Skriver ny tekst til en fil, erstatte eksisterende innhold
Set-Content -Path "hilsen.txt" -Value "Hei, Verden!"

# Legger til tekst til eksisterende innhold
Add-Content -Path "hilsen.txt" -Value "Hallo igjen!"

# Eksempel på innhold i 'hilsen.txt' etter alle kommandoene har kjørt
Get-Content "hilsen.txt"
```
Output:
```
Hei, Verden!
Hallo igjen!
```

## Dypdykk
Opprinnelig i DOS og tidlige Windows-versjoner ble tekstfiler ofte skrevet med batch-skript og `echo` kommandoer. PowerShell tilbyr mer avanserte og fleksible cmdlets for samme oppgave. Alternativene, som `StreamWriter` klassen fra .NET-rammeverket, tilbyr større kontroll over I/O-ytelse og encoding. `Set-Content` og `Add-Content` arbeider med pipeline-objekter og kan skrive uten å måtte laste hele filen inn i minnet, som er nyttig for store filer.

## Se Også
- [About Out-File (Microsoft Documentation)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [About Set-Content (Microsoft Documentation)](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Set-Content)
- [About Add-Content (Microsoft Documentation)](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Add-Content)
- [PowerShell File I/O (small tidbit on .NET StreamWriter class)](https://devblogs.microsoft.com/scripting/understanding-streams-redirection-and-write-host-in-powershell)
