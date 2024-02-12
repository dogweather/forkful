---
title:                "Lavorare con JSON"
aliases: - /it/powershell/working-with-json.md
date:                  2024-02-03T19:23:29.609170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'integrazione di PowerShell con JSON (JavaScript Object Notation) riguarda l'analisi (lettura) e la generazione (scrittura) di dati JSON, un formato comune per lo scambio di dati sul web. I programmatori lavorano con JSON per interagire con le API web, i file di configurazione o per facilitare lo scambio di dati tra diversi linguaggi e piattaforme data la sua natura leggera e indipendente dal linguaggio.

## Come fare:

### Analizzare JSON

Per leggere o analizzare JSON in PowerShell, puoi usare il cmdlet `ConvertFrom-Json`. Data una stringa JSON, questo cmdlet la converte in un oggetto PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Output di esempio:

```
John Doe
```

Questo esempio dimostra come analizzare una semplice stringa JSON per accedere alle proprietà dell'oggetto risultante.

### Generare JSON

Per generare JSON a partire da un oggetto PowerShell, puoi usare il cmdlet `ConvertTo-Json`. Questo è utile per preparare i dati da inviare a un servizio web o da salvare in un file di configurazione.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Output di esempio:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Questo frammento di codice crea un oggetto PowerShell e poi lo converte in una stringa JSON.
