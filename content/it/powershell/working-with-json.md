---
title:                "Lavorare con json"
html_title:           "PowerShell: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 
Lavorare con JSON è una tecnica comune per gestire dati strutturati in JavaScript Object Notation (JSON) su diversi framework e piattaforme. I programmatori spesso usano JSON per scambiare dati tra due applicazioni diverse o per salvare dati in un formato leggibile e leggero.

## Come fare: 
```PowerShell
# Creare un oggetto JSON con alcuni dati di esempio
$json = @"
{
	"nome": "Marco",
	"cognome": "Rossi",
	"età": 30,
	"hobby": ["programmazione", "calcio", "viaggi"]
}
"@

# Convertire l'oggetto JSON in formato testo
$json | ConvertTo-Json

# Leggere un file JSON 
$jsonFile = Get-Content -Path "C:\percorso\al\file.json" | ConvertFrom-Json

# Accedere ai dati all'interno del file JSON 
$jsonFile.nome
$jsonFile.cognome
$jsonFile.età
$jsonFile.hobby

# Esportare un file JSON 
$jsonFile | ConvertTo-Json | Out-File -Path "C:\nuovo\percorso\al\file.json"
```

## Approfondimento: 
L'uso di JSON è diventato popolare grazie alla sua semplicità e flessibilità rispetto ad altri formati di dati come XML. Altri modi per lavorare con dati strutturati includono CSV, YAML e HCL. PowerShell offre anche dei comandi specifici per lavorare con dati XML, come `ConvertTo-Xml` e `ConvertFrom-Xml`. 

## Vedi anche: 
- [Documentazione ufficiale su JSON in PowerShell](https://docs.microsoft.com/powershell/module/Microsoft.PowerShell.Utility/ConvertFrom-Json?view=powershell-7.1)
- [Integrazione di JSON in PowerShell](https://blog.purestorage.com/powershell-and-json-basic-usage-and-filtering-a-json-array/)