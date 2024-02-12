---
title:                "Lavorare con i CSV"
aliases:
- /it/powershell/working-with-csv/
date:                  2024-02-03T19:20:39.251429-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con i file CSV (Comma-Separated Values, o Valori Separati da Virgola) è un'operazione comune per gestire e manipolare dati in una forma strutturata e tabulare. Gli sviluppatori spesso eseguono questa operazione per importare, esportare o manipolare dati in modo efficiente per varie applicazioni, come l'analisi dei dati, la creazione di report o persino per alimentare applicazioni web.

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
