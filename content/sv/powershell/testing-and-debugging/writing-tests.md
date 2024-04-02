---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:40.125826-07:00
description: "Att skriva tester i PowerShell inneb\xE4r att skapa skript som automatiskt\
  \ validerar funktionaliteten i din PowerShell-kod, f\xF6r att s\xE4kerst\xE4lla\
  \ att den beter\u2026"
lastmod: '2024-03-13T22:44:38.129427-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i PowerShell inneb\xE4r att skapa skript som automatiskt\
  \ validerar funktionaliteten i din PowerShell-kod, f\xF6r att s\xE4kerst\xE4lla\
  \ att den beter\u2026"
title: Skriva tester
weight: 36
---

## Vad & Varför?

Att skriva tester i PowerShell innebär att skapa skript som automatiskt validerar funktionaliteten i din PowerShell-kod, för att säkerställa att den beter sig som förväntat. Programmerare gör detta för att upptäcka buggar tidigt, förenkla kodunderhåll och säkerställa att kodändringar inte oavsiktligt bryter befintlig funktionalitet.

## Hur man gör:

PowerShell har inte ett inbyggt testramverk, men Pester, en populär tredjepartsmodul, används ofta för att skriva och köra tester. Så här kommer du igång med Pester för att testa dina PowerShell-funktioner.

Först, installera Pester om du inte redan har gjort det:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Nästa, anta att du har en enkel PowerShell-funktion du vill testa, sparad som `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

För att testa den här funktionen med Pester, skapa ett testskript med namnet `MyFunction.Tests.ps1`. I detta skript, använd Pesters `Describe` och `It` block för att definiera testfallen:

```powershell
# Importera funktionen som ska testas
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tester" {
    It "Multiplicerar tal med 2 när ingen multiplikator ges" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multiplicerar korrekt tal med angiven multiplikator" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

För att köra testerna, öppna PowerShell, navigera till katalogen som innehåller ditt testskript, och använd kommandot `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Exempelutmatning kommer se ut så här, vilket indikerar om dina tester har passerat eller misslyckats:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\path\till\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tester slutförda på 204ms
Tester Passerade: 2, Misslyckades: 0, Hoppade över: 0 EjKörda: 0
```

Denna utmatning visar att båda testerna har passerat, vilket ger dig förtroende för att din `Get-MultipliedNumber` funktion beter sig som förväntat under de scenarier du har testat.
