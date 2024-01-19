---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?
Confrontare due date significa determinare quale data è anteriore, successiva o se sono uguali. I programmatori lo fanno per controllare o manipolare sequenze temporali in codice. 

## Come fare:
Ecco un esempio di come confrontare due date in PowerShell:

```PowerShell 
# Creazione di due date
$data1 = Get-Date
$data2 = Get-Date -Date '2022-12-31'

# Confronto delle date
if ($data1 -gt $data2) {
    Write-Output 'La data1 è successiva alla data2'
} elseif ($data1 -lt $data2) {
    Write-Output 'La data1 è anteriore alla data2'
} else {
    Write-Output 'Le due date sono uguali'
}
```
Output:

```PowerShell 
'La data1 è anteriore alla data2'
```
## Approfondimento
Historicalmente, PowerShell non ha avuto comandi di confronto di date incorporati fino alla versione 2.0. Alternativamente, si può anche confrontare le date convertendole in un timestamp Unix e confrontando i numeri.

In termini di implementazione, PowerShell sfrutta il metodo DateTime.CompareTo() di .NET per eseguire il confronto delle date. Ecco un esempio:

```PowerShell 
# Creazione di due date
$data1 = Get-Date
$data2 = Get-Date -Date '2022-12-31'

# Confronto delle date utilizzando CompareTo
$result = $data1.CompareTo($data2)

if ($result -gt 0) {
    Write-Output 'La data1 è successiva alla data2'
} elseif ($result -lt 0) {
    Write-Output 'La data1 è anteriore alla data2'
} else {
    Write-Output 'Le due date sono uguali'
}
```

## Vedi Anche
- Guida a PowerShell di Microsoft: https://docs.microsoft.com/it-it/powershell/
- Confronto delle date in .NET: https://docs.microsoft.com/it-it/dotnet/api/system.datetime.compareto?view=net-6.0
- Guida alla manipolazione delle date in PowerShell: https://adamtheautomator.com/powershell-get-date/