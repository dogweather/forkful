---
date: 2024-01-20 17:56:31.647075-07:00
description: "How to: PowerShell ha introdotto il concetto di script parametrizzati\
  \ dagli albori per offrire flessibilit\xE0 e riutilizzo del codice. Altri linguaggi\
  \ usano\u2026"
lastmod: '2024-04-05T21:53:44.418538-06:00'
model: gpt-4-1106-preview
summary: "PowerShell ha introdotto il concetto di script parametrizzati dagli albori\
  \ per offrire flessibilit\xE0 e riutilizzo del codice."
title: Lettura degli argomenti della riga di comando
weight: 23
---

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
