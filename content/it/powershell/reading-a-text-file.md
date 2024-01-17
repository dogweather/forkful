---
title:                "Lettura di un file di testo"
html_title:           "PowerShell: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lettura di un file di testo in PowerShell 

## Che cos'è e perché si fa? 
La lettura di un file di testo in PowerShell è il processo di apertura e lettura dei contenuti di un file di testo. I programmatori spesso effettuano questa operazione per ottenere informazioni o dati specifici dal file di testo che possono essere elaborati o utilizzati in ulteriori script. 

## Come fare: 
Ecco un esempio di codice in PowerShell che mostra come leggere un file di testo e stampare il suo contenuto sulla console: 

```
$file = Get-Content -Path C:\Users\NomeUtente\Documenti\mio_file.txt
$file
```
Questo codice utilizza il cmdlet Get-Content per ottenere il contenuto del file di testo specificato e lo assegna alla variabile $file. Successivamente, il contenuto viene stampato sulla console utilizzando la variabile stessa. 

Ecco un altro esempio che mostra come leggere un file di testo e contare il numero di righe che contiene: 

```
$file = Get-Content -Path C:\Users\NomeUtente\Documenti\mio_file.txt
$file.Count 
```
In questo caso, utilizziamo il cmdlet Count per contare il numero di elementi nella variabile $file, che corrisponde al numero di righe del file di testo. 

## Approfondimento: 
La lettura di un file di testo è una funzionalità fondamentale in quasi tutti i linguaggi di programmazione e anche in PowerShell. Originariamente, per leggere un file di testo in PowerShell, si utilizzava il cmdlet [System.IO.File] :: ReadAllText (). Tuttavia, con l'introduzione del cmdlet Get-Content in PowerShell 3.0, questo processo è diventato molto più semplice e intuitivo. 

Una delle alternative alla lettura di un file di testo in PowerShell è l'utilizzo del cmdlet foreach, che consente di iterare su ogni riga del file e eseguire le operazioni desiderate su ciascuna di esse. 

## Vedi anche: 
Documentazione ufficiale di Microsoft su [Get-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/get-content?view=powershell-7)