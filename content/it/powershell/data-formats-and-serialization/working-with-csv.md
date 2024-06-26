---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:39.251429-07:00
description: "Come fare: Per leggere da un file CSV, utilizzare il cmdlet `Import-Csv`.\
  \ Questo cmdlet legge il file e lo converte in oggetti personalizzati di\u2026"
lastmod: '2024-03-13T22:44:43.664552-06:00'
model: gpt-4-0125-preview
summary: Per leggere da un file CSV, utilizzare il cmdlet `Import-Csv`.
title: Lavorare con i CSV
weight: 37
---

## Come fare:


### Leggere un File CSV
Per leggere da un file CSV, utilizzare il cmdlet `Import-Csv`. Questo cmdlet legge il file e lo converte in oggetti personalizzati di PowerShell per ogni riga.

```powershell
# Importazione di un file CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# Visualizzare il contenuto
$data
```

**Esempio di Output:**

```
Name    Age    City
----    ---    ---
John    23     New York
Doe     29     Los Angeles
```

### Scrivere su un File CSV
Al contrario, per scrivere dati in un file CSV, si utilizza il cmdlet `Export-Csv`. Questo cmdlet prende oggetti in input e li converte in un formato CSV.

```powershell
# Creazione di un oggetto da esportare
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Esportazione in un file CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Dopo l'esecuzione, viene creato un file chiamato `new_users.csv` con i dati forniti.

### Filtrare e Manipolare il Contenuto dei CSV
Per filtrare o manipolare i dati da un file CSV, utilizzare le capacità di manipolazione degli oggetti di PowerShell. Ad esempio, per selezionare solo gli utenti sopra una certa età e di una specifica città:

```powershell
# Importazione e filtraggio dei dati
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Visualizzare i dati filtrati
$filteredData
```

**Esempio di Output:**

```
Name    Age    City
----    ---    ---
Doe     29     Los Angeles
```

### Utilizzo di Librerie di Terze Parti
Sebbene i cmdlet nativi di PowerShell siano generalmente sufficienti per le operazioni comuni, le operazioni più complesse potrebbero trarre vantaggio dall'utilizzo di librerie o strumenti di terze parti. Tuttavia, per la manipolazione standard dei CSV, come la lettura, la scrittura, il filtraggio o l'ordinamento, i cmdlet integrati di PowerShell come `Import-Csv` e `Export-Csv` offrono di solito una funzionalità robusta senza la necessità di librerie aggiuntive.
