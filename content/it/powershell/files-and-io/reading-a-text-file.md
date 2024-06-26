---
date: 2024-01-20 17:54:57.815673-07:00
description: 'How to: Ecco come leggere un file di testo con PowerShell in modo semplice
  e diretto.'
lastmod: '2024-03-13T22:44:43.659731-06:00'
model: gpt-4-1106-preview
summary: Ecco come leggere un file di testo con PowerShell in modo semplice e diretto.
title: Lettura di un file di testo
weight: 22
---

## How to:
Ecco come leggere un file di testo con PowerShell in modo semplice e diretto:

```PowerShell
# Usando Get-Content
$contenuto = Get-Content -Path "C:\percorso\del\file.txt"
$contenuto

# Usando [System.IO.File]::ReadAllText
$contenuto = [System.IO.File]::ReadAllText("C:\percorso\del\file.txt")
$contenuto
```

Output di esempio:

```
Prima riga del file
Seconda riga del file
Terza riga del file
```

## Deep Dive
Get-Content è un cmdlet PowerShell che legge facilmente file di testo. Storico dal 2006, è parte della gestione file integrata di PowerShell. Alternative includono l'uso di .NET con [System.IO.File], che offre più controllo su come i dati sono letti (es. file grandi). Dettagli implementativi: Get-Content legge riga per riga (ideale per scripting), .NET è meglio per grosse prestazioni o complesse manipolazioni.

## See Also
- Documentazione ufficiale Get-Content: [Microsoft Docs Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- Guida .NET per lettura file: [Microsoft Docs System.IO.File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-6.0)
- Articolo sulla manipolazione di file di testo: [Manipolazione file di testo in PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-data-basics-file-based-data/)
