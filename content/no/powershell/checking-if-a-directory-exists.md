---
title:                "Sjekker om en mappe eksisterer"
html_title:           "PowerShell: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekk om en mappe eksisterer er å sjekke om en gitt mappe finnes på datamaskinen din. Dette er en viktig del av programmering fordi det gjør det mulig å kontrollere om et nødvendig sted for å lagre eller hente data er tilstede.

## Hvordan:
```PowerShell
# Sjekk om en mappe eksisterer
Test-Path C:\Users\Brukernavn\Documents

# Eksempel på utskrift:
True  
```

```PowerShell
# Sjekk om en mappe ikke eksisterer
Test-Path C:\Users\Brukernavn\Bilder

# Eksempel på utskrift:
False  
```

## Dykk Dypere:
Sjekking av mappeneksistens har eksistert i programmering i lang tid, og er en viktig del av å sikre at nødvendig funksjonalitet er tilstede før programmet kjører. Alternativer til Test-Path kommandoen inkluderer å bruke FileSystemObject i VBScript, eller å bruke C# eller Java. 
Implementasjonen av Test-Path kommandoen i PowerShell innebærer sjekking av både sti og tillatelser for å avgjøre om en mappe eksisterer eller ikke. Manglende tillatelse kan føre til et feiltrinn uansett om mappen eksisterer eller ikke.

## Se også:
Offisiell dokumentasjon for Test-Path kommandoen: https://docs.microsoft.com/powershell/module/Microsoft.PowerShell.Management/Test-Path?view=powershell-7