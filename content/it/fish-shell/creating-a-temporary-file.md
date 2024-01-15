---
title:                "Creazione di un file temporaneo"
html_title:           "Fish Shell: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile in diverse situazioni, ad esempio quando si vuole memorizzare temporaneamente dei dati o quando si vuole eseguire un'operazione senza influenzare il contenuto di un file esistente. Inoltre, l'utilizzo di file temporanei può essere uno strumento utile nel processo di sviluppo e debugging di un programma.

## Come fare

Per creare un file temporaneo in Fish Shell, è possibile utilizzare il comando `mktemp`. Questo comando genera un file temporaneo con un nome univoco e restituisce il percorso completo del file appena creato. Ecco un esempio di come utilizzare il comando `mktemp`:

```Fish Shell
$ mktemp
/tmp/tmp.oT9S2P6fxC
```

In questo esempio, viene creato un file temporaneo dal nome "tmp.oT9S2P6fxC" nella directory "/tmp". Possiamo anche specificare un percorso diverso per il file temporaneo utilizzando l'opzione `-p`. Ad esempio:

```Fish Shell
$ mktemp -p ~/Documents
/Users/username/Documents/tmp.x7awIJsimf
```

In questo caso, il file temporaneo viene creato nella directory "Documents" della home directory dell'utente.

## Approfondimento

Il comando `mktemp` utilizza per default un template per generare il nome del file temporaneo, ma possiamo anche specificare il nome del file utilizzando l'opzione `-t`. Ad esempio:

```Fish Shell
$ mktemp -t mytempfile
/tmp/mytempfile
```

In questo caso, il nome del file temporaneo verrà prefissato con il testo "mytempfile". Possiamo anche specificare un suffisso utilizzando l'opzione `-s`, ad esempio:

```Fish Shell
$ mktemp -t mytempfile -s .txt
/tmp/mytempfile.txt
```

Inoltre, possiamo specificare più opzioni per personalizzare il nome e la posizione del file temporaneo desiderato.

## Vedi anche

Per ulteriori informazioni sul comando `mktemp`, puoi consultare la documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/cmds/mktemp.html.

Puoi anche approfondire l'utilizzo dei file temporanei in Fish Shell e come possono essere utili durante lo sviluppo di script e programmi: https://fishshell.com/docs/current/tutorial.html#temporary-files.