---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til standardfeil (stderr) separerer feilmeldinger fra vanlig output. Programmere bruker dette for å lettere spore og håndtere feil.

## Hvordan:
```PowerShell
# Skriv en feilmelding til stderr
$Host.UI.WriteErrorLine("Dette er en feilmelding")

# Redirect stderr til en fil
$ErrorActionPreference = "SilentlyContinue"
$MyCommand 2> errorlog.txt

# Se på innholdet i errorlog.txt for feilmeldinger
Get-Content errorlog.txt
```
Eksempel output:
```
Dette er en feilmelding
```

## Dypdykk
Stderr kom fra Unix og ble tatt i bruk for å kjøre feil separat fra standard output (stdout). Alternativer inkluderer bruk av logging-rammeverk. PowerShell implementerer dette gjennom systemet sitt 'streams', hvor stderr er strøm #2.

## Se Også
- Microsofts offisielle dokumentasjon om redirection: https://docs.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-redirection?view=powershell-7.2
- En artikkel om feilhåndtering i PowerShell: https://devblogs.microsoft.com/scripting/understanding-non-terminating-errors-in-powershell/
