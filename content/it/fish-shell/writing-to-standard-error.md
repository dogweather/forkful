---
title:    "Fish Shell: Scrivere su standard error"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è una pratica utile per visualizzare errori o avvisi specifici durante l'esecuzione di un programma. In questo modo, è possibile identificare e risolvere i problemi in modo tempestivo.

## Come fare

Per scrivere su standard error utilizzando Fish Shell, è possibile utilizzare il comando `echo` seguito dalla stringa da visualizzare e il simbolo `>` per indicare l'output su stderr. Ad esempio:

```Fish Shell
echo "Errore: il file non è stato trovato" >&2
```

Questo produrrà un output simile al seguente:

```Fish Shell
Errore: il file non è stato trovato
```

## Approfondisci

Scrivere su standard error è particolarmente utile quando si scrivono script o programmi più complessi, in quanto consente di visualizzare informazioni importanti che altrimenti potrebbero passare inosservate nell'output standard. Inoltre, utilizzando Fish Shell, è possibile anche eseguire il redirect dell'output di un comando specifico su stderr utilizzando `2>` seguito dall'output desiderato. Ad esempio:

```Fish Shell
ls -l file_non_esistente 2> errore.txt
```

Questo comando produrrà un file "errore.txt" contenente eventuali errori generati dall'esecuzione del comando `ls` su un file inesistente. Ciò è particolarmente utile quando si desidera registrare gli errori per analisi future.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial di Fish Shell su Codementor](https://www.codementor.io/@ericowens/how-to-use-fish-shell-1vsz7dkzbd)
- [Articolo su come scrivere a standard error su OMG! Ubuntu! (in italiano)](https://www.omgubuntu.co.uk/2020/02/write-to-stderr-linux)