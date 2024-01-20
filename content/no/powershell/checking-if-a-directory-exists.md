---
title:                "Sjekker om en katalog eksisterer"
html_title:           "PowerShell: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor? - What & Why?
Sjekke om en mappe eksisterer i PowerShell innebærer å validere for tilstedeværelsen av en bestemt mappe på systemet ditt. Denne operasjonen er sentral for å unngå feil og unødvendige krasj når man prøver å manipulere mapper som kanskje ikke eksisterer.

## Hvordan gjør man det - How to:

Her er et enkelt eksempel på hvordan du kan sjekke om en mappe eksisterer ved hjelp av PowerShell:

```PowerShell
if(Test-Path C:\FolderenMin)
{
  Write-Host "Mappen eksisterer."
}
else
{
  Write-Host "Mappen eksisterer ikke."
}
```

Ved å kjøre denne koden vil PowerShell returnere enten "Mappen eksisterer." eller "Mappen eksisterer ikke.", avhengig av mappens eksistens.

## Dypdykk - Deep Dive:
Måten å sjekke om mapper eksisterer på i tidligere versjoner av PowerShell og Command Prompt var litt forskjellig, men Test-Path cmdlet har gjort det mye enklere og mer robust i nyere versjoner av PowerShell. 

Selv om Test-Path er den mest brukte metoden, er det også andre alternativer tilgjengelig, som å bruke [System.IO.Directory]::Exists metoden i .NET-klassen.

```PowerShell
[System.IO.Directory]::Exists('C:\FolderenMin')
```
Denne koden vil returnere "True" hvis mappen eksisterer, og "False" i motsatt tilfelle.

## Se Også - See Also:
Her er noen nyttige lenker for deg som vil lære mer om PowerShell og bruk av stier og mapper:

1. Microsoft Docs - About Test-Path: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/test-path?view=powershell-7
2. Microsoft Docs - System.IO.Directory Class: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=net-5.0