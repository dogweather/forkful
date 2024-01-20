---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# PowerShell: Lesing av kommandolinjeargumenter 

## Hva & Hvorfor?
Kommandolinjeargumenter tillater interaksjon mellom brukeren og koden, hvor brukeren kan kontrollere kjøringen eller funksjonen av programmet. Utviklere bruker dette for å øke fleksibiliteten og tilpasningsdyktigheten i sine skript.

## Hvordan?
Her er et grunnleggende eksempel på hvordan man leser command-linjeargumenter i PowerShell. 

```PowerShell
Param(
  [Parameter(Mandatory=$true)]
  $Argument1,
  
  [Parameter(Mandatory=$false)]
  [Alias("V")]
  $Verbose=$false
)

# Resten av koden kommer her...
Write-Host "Argument funnet: $Argument1. Detaljert modus er $Verbose"
```
Noen eksempler på kjøring: 
```PowerShell 
> .\myScript.ps1 -Argument1 "Hei"
Argument funnet: Hei. Detaljert modus er False

> .\myScript.ps1 -Argument1 "Hei" -Verbose $true
Argument funnet: Hei. Detaljert modus er True
```
## Dyp Dykk
PowerShell sin handling av kommandolinjeargumenter har likheter til UNIX-baserte shell-script. Det finnes også alternativer til  `Param` nøkkelordet, som `$args` array variabelen, men de gir ikke like mye kontroll over hvilke argumenter som godtas. Gjennom deklarasjonen av spesifikke parametere kan vi håndtere input på en kontrollert og definert måte.

## Se Også 
2. [Detaljert Stack Overflow tråd om PowerShell argumenthåndtering](https://stackoverflow.com/questions/2157554/how-to-handle-command-line-arguments-in-powershell)