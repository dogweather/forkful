---
title:    "Fish Shell: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

La cancellazione di caratteri corrispondenti a un determinato modello può essere utile per pulire e organizzare i dati all'interno di un file o di una stringa. Inoltre, può essere utilizzato per effettuare rapidamente delle modifiche su un grande numero di elementi.

## Come fare

Per cancellare i caratteri in Fish Shell, possiamo utilizzare il flag -d seguito dal modello che vogliamo eliminare. Ad esempio:

```
Fish Shell v.3.1.2
ubuntu@localhost:~$ rm filename -d am
```

In questo esempio, stiamo eliminando tutti i caratteri che corrispondono al modello "am" dal file "filename". Possiamo anche utilizzare caratteri speciali come "?", "*" e "[]" per definire un modello più specifico.

```
Fish Shell v.3.1.2
ubuntu@localhost:~$ rm filename -d ????.txt
```

In questo caso, elimineremo tutti i file con estensione ".txt" che hanno un nome di 4 caratteri.

## Approfondimento

La cancellazione di caratteri basata su un modello è possibile grazie all'utilizzo di espressioni regolari. In poche parole, un'espressione regolare è una sequenza di caratteri che definisce un modello di ricerca. Questo permette di effettuare operazioni di ricerca e sostituzione avanzate all'interno di un file o di una stringa.

## Vedi anche

- [Fish Shell manuale utente - Cancella](https://fishshell.com/docs/current/cmds/cancel.html)
- [Tutorial di espressioni regolari in italiano](https://www.pittogrammi.it/espressioni-regolari/)
- [Guida pratica alle espressioni regolari in Fish Shell](https://medium.com/@kimberlykjesler/fish-shell-regular-expressions-309e4ac5f181)