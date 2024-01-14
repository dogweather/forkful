---
title:    "Fish Shell: Ricerca e sostituzione di testo"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Perché

La ricerca e la sostituzione di testo sono importanti per gli sviluppatori e gli utenti del Fish Shell poiché permettono di automatizzare il processo di modifica di testi ripetitivi. Inoltre, questa funzionalità può risparmiare tempo e aumentare l'efficienza nella scrittura dei comandi del terminale.

## Come fare

Per eseguire una ricerca e sostituzione di testo nel Fish Shell, è necessario utilizzare il comando `sed` (stream editor). Questo comando accetta due parametri: l'espressione regolare della stringa da cercare e la stringa di sostituzione. Ecco un esempio:

```
Fish Shell → sed ‘s/gatto/cane/’ <file.txt
```

In questo caso, il comando sostituisce ogni occorrenza della parola "gatto" con "cane" nel file specificato. È possibile utilizzare l'opzione `-i` per effettuare la sostituzione direttamente nel file senza dover creare un nuovo file.

È anche possibile utilizzare espressioni regolari più complesse per cercare e sostituire testo. Ad esempio, il comando seguente sostituirà il testo "hello" con "ciao" solo all'interno delle righe contenenti la parola "nome":

```
Fish Shell → sed ‘/nome/ s/hello/ciao/g’ <file.txt
```

## Approfondimento

La funzione di ricerca e sostituzione del Fish Shell utilizza l'utility `sed` che è comune a molti sistemi operativi UNIX. È possibile trovare una guida completa alle espressioni regolari utilizzando il comando `man sed` nel terminale.

Inoltre, il Fish Shell supporta anche espressioni regolari estese attraverso il comando `pcre`. Questo comando utilizza la sintassi delle espressioni regolari più avanzata che può essere utile per affrontare situazioni più complesse.

# Vedi anche

- [Guida alla ricerca e sostituzione di testo con sed](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html#Regular-Expressions)
- [Documentazione completa del Fish Shell](https://fishshell.com/docs/current/)
- [Espressioni regolari avanzate con pcre](https://www.pcre.org/)

Grazie per aver letto questo articolo sulla ricerca e la sostituzione di testo nel Fish Shell. Speriamo che ti sia stato utile e che tu possa utilizzare questa funzionalità per facilitare il tuo lavoro con il terminale. A presto!