---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:29.609170-07:00
description: "Come fare: Per leggere o analizzare JSON in PowerShell, puoi usare il\
  \ cmdlet `ConvertFrom-Json`. Data una stringa JSON, questo cmdlet la converte in\
  \ un\u2026"
lastmod: '2024-03-13T22:44:43.663561-06:00'
model: gpt-4-0125-preview
summary: Per leggere o analizzare JSON in PowerShell, puoi usare il cmdlet `ConvertFrom-Json`.
title: Lavorare con JSON
weight: 38
---

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
