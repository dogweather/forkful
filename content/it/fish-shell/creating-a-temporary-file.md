---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Creare un file temporaneo è un'operazione che consiste nell'instaurare un file di transizione, esistente solo per la durata di un determinato processo o sessione. I programmatori lo fanno per gestire dati transitori senza consumare memoria o risorse preziose nel dispositivo principale.

## Come fare:

Nella "Fish Shell", possiamo utilizzare il comando `mktemp` per creare un file temporaneo. Vediamo un esempio:

```Fish Shell
set fileTemporaneo (mktemp)
echo "Ciao Mondo" > $fileTemporaneo
cat $fileTemporaneo
```

L'output sarà:

```Fish Shell
Ciao Mondo
```

## Approfondimento

Storicamente, l'uso di file temporanei ha aiutato i programmatori a gestire meglio le limitazioni di memoria dei sistemi più vecchi. Anche se le moderne macchine possono gestire quantità di dati significativamente maggiori, l'uso di file temporanei rimane una pratica comune per una programmazione efficiente.

Un'alternativa alla creazione di un file temporaneo è l'uso di variabili. Tuttavia, questi consumano spazio in memoria, rendendo i file temporanei un'opzione preferibile quando si gestiscono grandi quantità di dati.

In Fish Shell, il comando `mktemp` crea per default un file temporaneo nel formato `/tmp/tmp.XXXXXXXXXX` con un nome univoco generato in maniera casuale.

## Vedi Anche

Per approfondimenti sulla Fish Shell e su come gestire i file, consulta i seguenti link:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Working with temporary files in fish shell](https://fishshell.com/docs/current/tutorial.html#temp-files)