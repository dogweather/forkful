---
title:                "Lettura degli argomenti della riga di comando"
aliases: - /it/powershell/reading-command-line-arguments.md
date:                  2024-01-20T17:56:31.647075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando significa catturare le informazioni inserite quando un programma viene avviato. I programmatori lo fanno per personalizzare l'esecuzione del software senza cambiarne il codice.

## How to:
```PowerShell
# Esempio script: TestArgs.ps1
param (
    [string]$nome = "Mondo!",
    [int]$numero = 1
)

for($i = 0; $i -lt $numero; $i++) {
    Write-Host "Ciao $nome"
}

# Uso dello script dalla riga di comando:
# .\TestArgs.ps1 -nome "Italia" -numero 3

# Risultato:
# Ciao Italia
# Ciao Italia
# Ciao Italia
```

## Deep Dive
PowerShell ha introdotto il concetto di script parametrizzati dagli albori per offrire flessibilità e riutilizzo del codice. Altri linguaggi usano argomenti posizionali (`$args`) o variabili automatiche (`$PSBoundParameters`) ma PowerShell aggiunge il binding dichiarativo tramite `param()` per una lettura più intuitiva. È possibile anche implementare una logica avanzata che reagisce agli argomenti, come la convalida o i parametri dinamici.
