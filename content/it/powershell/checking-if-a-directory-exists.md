---
title:                "Verifica se una directory esiste"
html_title:           "PowerShell: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Controllare se una directory esiste è una pratica comune per i programmatori. Questa operazione viene eseguita per verificare l'esistenza di una determinata directory prima di eseguire altre azioni. Ad esempio, se si sta scrivendo uno script che deve creare un nuovo file in una directory, prima si vuole essere sicuri che la directory esista per evitare errori nell'esecuzione dello script.

## Come fare:

```PowerShell
# Utilizzando il cmdlet di PowerShell "Test-Path"
Test-Path -Path "C:\Users\Username\Desktop"

# Output atteso: "True" se la directory esiste, "False" se non esiste.

# Utilizzando l'operatore "-is" con l'oggetto DirectoryInfo
(Get-Item C:\Users\Username\Desktop) -is [System.IO.DirectoryInfo]

# Output atteso: "True" se la directory esiste, "False" se non esiste.
```

## Approfondimento:

Il controllo dell'esistenza di una directory esiste è una pratica comune già da molti anni nei linguaggi di programmazione. In precedenza, nei primi linguaggi di programmazione come il linguaggio C, era comune creare codice per verificare manualmente l'esistenza di una directory. Ad oggi, con l'uso di cmdlet o operatori specifici, questa operazione può essere eseguita in modo più semplice e veloce.

Altri linguaggi di programmazione, come Python, forniscono metodi specifici per verificare l'esistenza di una directory. Tuttavia, con PowerShell, è possibile utilizzare il cmdlet "Test-Path" che funziona sia per i sistemi operativi Windows che per i sistemi operativi UNIX.

Per quanto riguarda l'implementazione, sia il cmdlet "Test-Path" che l'operatore "-is" utilizzano l'API del sistema operativo per verificare l'esistenza della directory. Inoltre, con l'operatore "-is" è possibile controllare anche altri tipi di oggetti di sistema, non solo directory.

## Vedi anche:

- [Documentazione Microsoft su Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)
- [Documentazione Microsoft su operatori di PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7)