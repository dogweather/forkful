---
date: 2024-01-26 01:11:13.663719-07:00
description: "\xC5 organisere kode i funksjoner handler om \xE5 pakke inn kodesnutter\
  \ som utf\xF8rer spesifikke oppgaver og gi dem et navn. Det gj\xF8res for \xE5 gj\xF8\
  re koden\u2026"
lastmod: '2024-03-13T22:44:41.022889-06:00'
model: gpt-4-1106-preview
summary: "\xC5 organisere kode i funksjoner handler om \xE5 pakke inn kodesnutter\
  \ som utf\xF8rer spesifikke oppgaver og gi dem et navn. Det gj\xF8res for \xE5 gj\xF8\
  re koden\u2026"
title: Organisering av kode i funksjoner
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å pakke inn kodesnutter som utfører spesifikke oppgaver og gi dem et navn. Det gjøres for å gjøre koden gjenbrukbar, lesbar og vedlikeholdbar. I stedet for å skrive den samme koden på nytt, kaller du en funksjon. Vil du feilsøke eller oppgradere? Juster funksjonen uten å måtte sile gjennom hauger av skript.

## Hvordan:
La oss skrive en funksjon for å beregne summen av to tall. Enkelt, men det illustrerer poenget.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Kall funksjonen med 5 og 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "Summen er $sum"
```

Eksempel på utskrift:

```
Summen er 15
```

## Dykke dypere
Funksjoner i PowerShell, som i de fleste språk, er gammelt nytt. Vi har kompartmentalisert kode siden dagene av Fortran. Det handler om 'å ikke oppfinne hjulet på nytt'. Alternativer? Joda, skript eller cmdlets. Men de mangler ryddigheten og kontekstfølsomheten til funksjoner innen skript.

Implementasjon? Funksjoner kan være grunnleggende som vårt eksempel, eller komplekse med omfang, pipeline-inndata og mer. Ta `Avanserte Funksjoner`. De etterligner cmdlets med parametere som har attributter, som `[Parameter(Mandatory=$true)]`. Det er en smakebit av PowerShell sin fleksibilitet.

## Se også
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
