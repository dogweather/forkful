---
title:                "Lettura degli argomenti da linea di comando"
html_title:           "PowerShell: Lettura degli argomenti da linea di comando"
simple_title:         "Lettura degli argomenti da linea di comando"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Leggere gli argomenti dalla riga di comando è un'abilità essenziale per i programmatori. Consiste nel recuperare i valori inseriti dall'utente quando viene eseguito un programma attraverso il terminale. Questi argomenti possono essere utilizzati per modificare il comportamento del programma o per fornire informazioni aggiuntive.

## Come fare:
Il modo più semplice per leggere gli argomenti dalla riga di comando in PowerShell è utilizzando il parametro ```$args```, che contiene un'array di tutti gli argomenti inseriti dall'utente al momento dell'esecuzione. Si può accedere a questi argomenti utilizzando la loro posizione all'interno dell'array, ad esempio ```$args[0]``` per il primo argomento, ```$args[1]``` per il secondo e così via.

```PowerShell
# Esempio di codice per leggere gli argomenti dalla riga di comando
if ($args[0] -eq "help"){
  Write-Host "Benvenuti nel programma di aiuto"
}
```

Nell'esempio sopra, viene controllato se il primo argomento inserito è "help" e, in caso positivo, viene stampato un messaggio di benvenuto.

## Approfondimento:
In passato, i programmatori dovevano utilizzare strumenti esterni come ```getopt``` per leggere gli argomenti dalla riga di comando in modo strutturato. Tuttavia, grazie all'evoluzione di PowerShell, ora è possibile farlo in modo più semplice utilizzando il parametro ```$args```.

Un'alternativa a ```$args``` è l'utilizzo del comando ```Get-CommandLine``` che restituisce un oggetto contenente tutti gli argomenti della riga di comando separati in singoli elementi. Inoltre, è possibile specificare eventuali opzioni di formattazione per ottenere un output più dettagliato.

## Vedi anche:
- [Documentazione ufficiale di Microsoft su $args](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1)
- [Guida di riferimento di PowerShell per leggere gli argomenti dalla riga di comando](https://www.javatpoint.com/powershell-command-line-arguments)