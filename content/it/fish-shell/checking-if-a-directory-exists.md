---
title:    "Fish Shell: Verifica dell'esistenza di una cartella"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori potrebbero trovarsi nella situazione in cui devono verificare se una directory esiste prima di procedere con altre azioni, come ad esempio la creazione di un file o l'esecuzione di un comando. In questo articolo, impareremo come farlo utilizzando Fish Shell.

## Come fare

Per verificare se una directory esiste in Fish Shell, possiamo utilizzare il comando `test` con l'opzione `-d` che ci consente di specificare una directory.

```
Fish Shell test -d /percorso/della/directory
```

Se la directory esiste, il comando restituirà `true`, altrimenti restituirà `false`. Possiamo anche utilizzare questa verifica all'interno di una condizione `if` per eseguire diversi comandi in base al risultato.

```
if test -d /percorso/della/directory
   # se la directory esiste, esegui questi comandi
else
   # se la directory non esiste, esegui questi altri comandi
end
```

## Approfondimento

Il comando `test` può essere utilizzato non solo per verificare l'esistenza di una directory, ma anche di altri tipi di file. Possiamo utilizzare le opzioni `-e` per verificare se un file esiste, `-f` per controllare se è un normale file, o `-L` per verificare se è un link simbolico.

Inoltre, possiamo utilizzare anche il comando `true` o `false` come alternative più leggibili per il comando `test`. Ad esempio, possiamo scrivere `true -d /percorso/della/directory` anziché `test -d /percorso/della/directory`.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Esempi di utilizzo del comando test](https://www.linuxjournal.com/content/test-and-if-constructs)
- [Ulteriori informazioni sui comandi true e false](https://www.gnu.org/software/coreutils/manual/html_node/true-invocation.html)