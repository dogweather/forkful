---
title:                "Lavorare con JSON"
date:                  2024-01-19
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) è un formato leggero per lo scambio di dati. Lo usano i programmatori per semplificare la memorizzazione di strutture complesse in un formato facilmente leggibile e trasportabile tra diversi sistemi e linguaggi di programmazione.

## How to:

### Leggere un file JSON

```PowerShell
# Caricare e leggere un file JSON
$json = Get-Content -Path 'path/to/your/file.json' | ConvertFrom-Json

# Visualizzare il contenuto
$json
```

### Creare un nuovo oggetto JSON

```PowerShell
# Creare un oggetto con alcune proprietà
$object = [PSCustomObject]@{
    Nome = 'Mario'
    Cognome = 'Rossi'
    Età = 30
}

# Convertire l'oggetto in JSON
$json = $object | ConvertTo-Json

# Stampa il JSON
$json
```

### Modificare un oggetto JSON

```PowerShell
# Decodificare il JSON
$decodedObject = $json | ConvertFrom-Json

# Modificare una proprietà
$decodedObject.Età = 31

# Ricodificare in JSON
$modifiedJson = $decodedObject | ConvertTo-Json

# Visualizzare il JSON modificato
$modifiedJson
```

### Salvare un oggetto JSON in un file

```PowerShell
$modifiedJson | Out-File -FilePath 'path/to/your/updated_file.json'
```

## Deep Dive

JSON nasce negli anni 2000 come alternativa leggera a XML, semplificando il modo in cui i dati vengono scambiati e interpretati. In PowerShell, lavorare con JSON avviene principalmente attraverso due comandi: `ConvertFrom-Json` e `ConvertTo-Json`, che decodificano e codificano dati JSON rispettivamente. Questi strumenti si integrano in modo fluido nel versatile ecosistema di PowerShell, permettendo agli sviluppatori di trattare JSON come oggetti nativi del linguaggio.

## See Also

- Documentazione ufficiale PowerShell su JSON: [aka.ms/powershell-json](https://aka.ms/powershell-json)
- JSON.org, per approfondire il formato JSON: [json.org/json-it.html](http://json.org/json-it.html)
- W3Schools, per esempi pratici e tutorial su JSON: [w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp)
