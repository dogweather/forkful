---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

La conversione di una data in una stringa permette di formattare e visualizzare le date in modi differenti. Programmare questa funzionalità è essenziale quando si vuole personalizzare la visualizzazione delle date, come ad esempio in report o interfaccia utente.

## Come si fa:

in PowerShell si possono convertire le date in stringhe in molti modi. Ad esempio:

```PowerShell
# Creare una data
$Data = Get-Date 

# Convertire la data in una stringa
$StringaData = $Data.ToString() 

# Stampare la stringa
Write-Host $StringaData
```

Nell'esempio sopra, usiamo il metodo `.ToString()` per convertire l'oggetto data in una stringa. L'output sarà qualcosa come:

```PowerShell
05/03/2023 19:45:32
```

## Approfondimento

Historicamente, PowerShell ha sempre offerto la conversione di date in stringhe, mantenendo una coerente funzionalità nel tempo.

Ci sono molte altre alternative per convertire una data in una stringa stessa, come l'uso del metodo `.ToShortDateString()`, che restituirà un formato di più breve:

```PowerShell
$Data = Get-Date
$StringaData = $Data.ToShortDateString()
Write-Host $StringaData
```

L'output sarà simile a quello seguente:

```PowerShell
05/03/2023
```

In termini di implementazione, PowerShell si basa sulla libreria di classi .NET per le funzioni di conversione di data-stringa. Essa sfrutta i metodi di conversione esposti da `System.DateTime` come `.ToString()`, `.ToShortDateString()`, `.ToLongDateString()`, etc.

## Vedere Anche:

- [Documentazione Microsoft su Get-Date](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.utility/get-date)
- [Documentazione Microsoft su System.DateTime](https://docs.microsoft.com/it-it/dotnet/api/system.datetime)