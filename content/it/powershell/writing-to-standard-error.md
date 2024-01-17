---
title:                "Scrivere su standard error"
html_title:           "PowerShell: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Scrivere su standard error è un modo per mostrare messaggi di errore o di debug in modo diverso rispetto agli standard output. Questa pratica è molto utile per i programmatori poiché consente di separare chiaramente i messaggi di errore dai risultati del programma.

## Come fare:

Ecco un esempio di codice in PowerShell che mostra come scrivere su standard error:

```PowerShell
# Codice di esempio per scrivere su standard error
Write-Host "Messaggio di output"
Write-Error "Messaggio di errore"
```

L'output del programma sarà il seguente:

```
Messaggio di output
Messaggio di errore
```

## Approfondimento:

Lo standard error è un concetto che proviene dai primi sistemi operativi Unix/Linux. In quei sistemi, il terminale seguiva il principio "In caso di successo, non c'è output" e quindi gli errori venivano inviati al canale di output alternativo (standard error). Un'alternativa comune a scrivere su standard error è l'utilizzo di un file di log.

## Vedi anche:

[Documentazione sulle funzioni Write-Host e Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-host?view=powershell-7.1)

[Introduzione ai canali di output in PowerShell](https://www.tutorialspoint.com/powershell/powershell_output.htm)

[Risolvere i problemi di script in PowerShell utilizzando standard error](https://vladflore.com/2019/10/handling-script-failures-with-powershell-standard-error-stream/)