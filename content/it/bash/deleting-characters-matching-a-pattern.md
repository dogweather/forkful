---
title:    "Bash: Eliminazione di caratteri corrispondenti ad un modello"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Cancellare caratteri che corrispondono a un determinato modello può essere un'operazione utile quando si lavora con testi o dati che contengono informazioni indesiderate. Ad esempio, può essere necessario eliminare tutte le parole che iniziano con una determinata lettera o tutti i numeri presenti in una stringa. Con il Bash scripting, è possibile automatizzare questo processo per risparmiare tempo e fatica.

## Come fare

Per cancellare caratteri che corrispondono a un determinato pattern in Bash, è possibile utilizzare il comando `sed`. Questo comando è estremamente versatile e può essere utilizzato per modificare, eliminare o sostituire porzioni di testo in un file o in una variabile.

Di seguito è riportato un esempio di codice che utilizza il comando `sed` per eliminare tutte le parole che iniziano con la lettera "a":

``` Bash
# Dichiarazione di una variabile contenente il testo
testo="La casa è grande e accogliente. Anna ama il giardino."

# Utilizzo del comando sed per eliminare tutte le parole che iniziano con "a"
nuovo_testo=$(echo $testo | sed 's/\b[aA][a-zA-Z]*\b//g')

# Output
echo $nuovo_testo
La è e . ama il .
```

In questo esempio, la variabile `testo` contiene una stringa con diverse parole che iniziano con la lettera "a". Invece, la variabile `nuovo_testo` contiene la stessa stringa senza alcuna parola che inizia con "a". 

Ciò è possibile grazie all'utilizzo del pattern `\b[aA][a-zA-Z]*\b` all'interno del comando `sed`. Questo pattern corrisponde a qualsiasi parola che inizia con "a" seguita da una o più lettere, ed è circondato da dei "bordi" che indicano il limite delle parole all'interno della stringa. In questo modo, vengono catturate solo le parole che iniziano con "a" e non vengono incluse quelle che contengono la lettera "a" in altre posizioni.

## Approfondimento

Il comando `sed` può essere utilizzato in diversi modi per cancellare caratteri che corrispondono a un determinato pattern. Inoltre, è possibile combinare più comandi `sed` in una singola espressione per ottenere risultati ancora più specifici.

È importante notare che il comando `sed` non modifica il file originale, ma restituisce un output sulla shell che può essere catturato e assegnato a una variabile o reindirizzato in un file.

Per saperne di più su questo comando e su tutte le sue funzionalità, si consiglia di consultare la documentazione ufficiale o altri tutorial disponibili online.

## Vedi anche

- [Documentazione ufficiale di sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial su sed di The Linux Documentation Project](https://tldp.org/LDP/abs/html/textproc.html#LOCATEANDREMOVE)
- [Esempi pratici di utilizzo di sed](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)