---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creazione di File Temporanei in PowerShell

## Cosa e Perché?

La creazione di un file temporaneo consiste nell'instanziare un file destinato all'uso temporaneo. I programmatori lo fanno per conservare in modo temporaneo i dati che possono essere necessari per la durata di una sessione o per processi intermedi.

## Come fare:

Per creare un file temporaneo in PowerShell, possiamo utilizzare il cmdlet `New-TemporaryFile`. Ecco un esempio:

```PowerShell
$tempFile = New-TemporaryFile
Write-Host "Il file temporaneo è stato creato al percorso: $tempFile"
```

L'output sarà simile a:

```PowerShell
Il file temporaneo è stato creato al percorso: C:\Users\Utente\AppData\Local\Temp\tmp67BF.tmp
```

## Approfondisco

Creare file temporanei è una pratica storica della programmazione. Nasce nei primi computer mainframe per gestire le limitazioni delle memorie piccole.

Sono presenti molte alternative alla creazione di file temporanei, tra cui l'uso di cache in-memory, l'uso di database temporanei o l'archiviazione in cloud. La scelta migliore dipende dal problema specifico che stai cercando di risolvere.

Il cmdlet ` New-TemporaryFile` in PowerShell genera una cartella temporanea di sistema per creare il file con un nome univoco. Utilizza `Path.GetRandomFileName()` per generare un nome casuale per il file ed assicurare che non ci siano conflitti di nomi.

## Vedi anche

Per ulteriori informazioni sulla gestione dei file temporanei, consultate i seguenti link:

- Documentazione Microsoft su New-TemporaryFile: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile

- Esempi di utilizzo dei file temporanei: https://www.pdq.com/blog/powershell-guide-temporary-files