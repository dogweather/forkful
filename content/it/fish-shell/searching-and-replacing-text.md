---
title:                "Fish Shell: Ricerca e sostituzione di testo"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Sembra banale, ma la ricerca e la sostituzione di testo sono essenziali per ogni programmatore. Con l'utilizzo del Fish Shell, questa attività diventa ancora più semplice e veloce. In questo post, impareremo come utilizzare questa funzionalità e approfondiremo la sua implementazione.

## Come fare

Per prima cosa, apri il terminale e assicurati di avere installato Fish Shell. Successivamente, utilizza il comando `sed` per cercare e sostituire il testo desiderato. Ad esempio, se vogliamo sostituire tutte le occorrenze della parola "happy" con "laughing" in un file di testo, possiamo usare il seguente comando:
 
```Fish Shell
sed -i 's/happy/laughing/g' test.txt
```

Questo comando sostituirà tutte le occorrenze della parola "happy" con "laughing" nel file "test.txt" e sovrascriverà il file originale con le modifiche. Se si desidera solo visualizzare il risultato senza modificare il file originale, è possibile utilizzare il flag `-e`:

```Fish Shell
sed -e 's/happy/laughing/g' test.txt
```

Inoltre, è possibile utilizzare espressioni regolari per rendere la ricerca e la sostituzione ancora più precise. Ad esempio, se vogliamo sostituire solo "happy" quando seguito da una vocale, possiamo utilizzare il seguente comando:

```Fish Shell
sed -i 's/happyV/laughingV/g' test.txt
```

Dove "V" rappresenta una vocale qualsiasi. Questo comando sostituirà, ad esempio, "happy" con "laughing", ma non "happened". 

## Approfondimento

La funzione di ricerca e sostituzione di Fish Shell è basata sul comando `sed`, che significa "stream editor". Questo comando viene utilizzato per modificare e formattare i file di testo utilizzando espressioni regolari. Nel nostro esempio, il flag `-i` viene utilizzato per modificare il file originale, ma se si omette questo flag, il risultato verrà visualizzato solo nel terminale.

Inoltre, oltre all'utilizzo delle espressioni regolari, è possibile utilizzare anche i comandi `grep` e `awk` per organizzare e manipolare il testo prima di sostituirlo. Queste funzioni consentono una maggiore personalizzazione nel processo di ricerca e sostituzione.

## Vedi anche

- [Documentazione Fish Shell](https://fishshell.com/docs/current/)
- [Guida completa a `sed`](https://www.gnu.org/software/sed/manual/sed.html#Introduction)
- [Utilizzo dell'espressione regolare in `sed`](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)