---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Få den Nåværende Datoen i PowerShell: En Kort Introduksjon

## Hva & Hvorfor?

Å få den nåværende datoen i kode vil si å hente inn dagens dato og tidspunkt. Denne funksjonen er viktig i mange oppgaver, for eksempel for å lage tidsstempler eller for å planlegge fremtidige hendelser.

## Hvordan:

Det er utrolig enkelt å få den nåværende datoen i PowerShell. Bare bruk kommandoen `$Date = Get-Date`.

```PowerShell
$Date = Get-Date
echo $Date
```
Output:
```  
Thursday, May 13, 2021 8:30:50 PM
```
Du kan også formatere datoen til en mer lesbar eller kompakt form. For eksempel:

```PowerShell
$Date = Get-Date -Format "yyyy-MM-dd"
echo $Date
```
Output:  

```  
2021-05-13
```
## Dypdykk

Få Dagen Dato i PowerShell har vært mulig siden versjon 5. `Get-Date` er en del av PowerShell sin standardbibliotek, og er derfor tilgjengelig i alle PowerShell-miljøer.

Alternativer til `Get-Date` inkluderer `$Now = [DateTime]::Now` og `$UtcNow = [DateTime]::UtcNow`, som gir den nåværende dato og tid i den lokale og universelle tidszonen hhv. Disse metodene bruker .NET's DateTime klasse i stedet for PowerShell's innebygde funksjoner.

Kodingen av `Get-Date` er avhengig av operativsystemet for å få sin informasjon. PowerShell ber OS om den nåværende tid og dato, og formatterer den deretter basert på innstillingene dine.

## Se ogs

- [Offisiell Microsoft Get-Date Dokumentasjon](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell DateTime Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0) 
- [StackOverflow Diskusjoner om Get-Date](https://stackoverflow.com/questions/tagged/powershell+get-date)