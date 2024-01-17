---
title:                "Utskrift av feilsøkningsutdata"
html_title:           "PowerShell: Utskrift av feilsøkningsutdata"
simple_title:         "Utskrift av feilsøkningsutdata"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Debug utskrift er en metode som lar utviklere vise informasjon om et program mens det kjører, for å finne feil og forstå hvordan koden utføres. Dette er en viktig del av programmering for å sikre feilfri kode og effektiv debugging-prosess.

## Hvordan:
For å skrive ut debug informasjon i PowerShell, kan du bruke `Write-Host` kommandoen. Dette vil skrive ut informasjonen rett til terminalen. Du kan også bruke `Write-Debug` for å spesifisere at utskriften kun skal vises når debugging er aktivert.

```PowerShell
Write-Host "Denne teksten vil bli skrevet ut som debug informasjon."
Write-Debug "Dette ble utskrevet under debugging."
```
Output:
```
Denne teksten vil bli skrevet ut som debug informasjon.
```
For å aktivere debugging modus, bruk `Set-PSDebug -Trace 1` kommandoen. Dette vil vise all debug informasjon som er lagt til i koden.

## Dypdykk:
Debugging er en viktig del av programmering og har eksistert siden de tidlige dagene med datamaskiner. Tidligere ble det gjort ved å skrive ut informasjonen på papir eller skrive den til en fil. Alternativer til å bruke `Write-Debug` inkluderer å bruke `Write-Verbose` for mer generell informasjon eller `Write-Warning` for advarsler om potensielle feil.

Implementasjonen av debugging i PowerShell er basert på .NET debugging og tilbyr mange av de samme funksjonene og mulighetene som .NET-debuggeren.

## Se også:
- [Microsoft Docs: PowerShell Debugging](https://docs.microsoft.com/en-us/powershell/scripting/developer/windows-powershell/how-to-debug-powershell-scripts?view=powershell-7)
- [Stack Overflow: Debugging in PowerShell](https://stackoverflow.com/questions/48432600/how-to-debug-powershell-code/48432846)