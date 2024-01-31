---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Lavorare con i CSV (Comma-Separated Values, Valori Separati da Virgola) consente di manipolare dati tabellari semplici. I programmatori usano CSV per importare, esportare e trasformare dati fra differenti sistemi e applicazioni senza complicazioni.

## Come si fa:
```PowerShell
# Importazione di un file CSV in un oggetto
$datiCSV = Import-Csv -Path "percorsodelfile.csv"

# Visualizzazione dei dati importati
$datiCSV

# Creazione di un nuovo CSV a partire da oggetto
$datiCSV | Export-Csv -Path "nuovofile.csv" -NoTypeInformation

# Aggiunta di un nuovo record a un CSV esistente
$nuovoRecord = @{
    Nome = "Mario"
    Cognome = "Rossi"
    Età = 30
}
$datiCSV += New-Object PSObject -Property $nuovoRecord
$datiCSV | Export-Csv -Path "percorsodelfile.csv" -NoTypeInformation
```
Output esemplificativo per la visualizzazione dei dati importati:
```
Nome   Cognome   Età
----    --------   ---
John     Doe          23
Jane     Smith     29
```

## Approfondimento:
Il formato CSV esiste dal 1972, diventando uno standard de facto per lo scambio di dati tabellari. Alternative includono JSON e XML, che gestiscono dati più complessi ma sono più verbosi. In PowerShell, si gestiscono CSV con oggetti nativi `Import-Csv` e `Export-Csv`, ottimizzati per la semplicità e l'integrazione con il sistema.

## Vedi anche:
- Documentazione Microsoft su `Import-Csv`: [Import-Csv (Microsoft.PowerShell.Utility)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv?view=powershell-7.1)
- Documentazione Microsoft su `Export-Csv`: [Export-Csv (Microsoft.PowerShell.Utility)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv?view=powershell-7.1)
- Tutorial su come analizzare e visualizzare dati CSV con PowerShell: [Parsing and visualizing CSV Data](https://medium.com/@peterziegler/parsing-and-visualizing-csv-data-with-powershell-9bf9e535fdf6)
