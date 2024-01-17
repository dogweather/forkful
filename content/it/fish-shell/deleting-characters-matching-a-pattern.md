---
title:                "Eliminazione di caratteri che corrispondono a un modello"
html_title:           "Fish Shell: Eliminazione di caratteri che corrispondono a un modello"
simple_title:         "Eliminazione di caratteri che corrispondono a un modello"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Che cos'è e perché?

Cancellare caratteri che corrispondono a uno schema è una pratica comune tra i programmatori per rimuovere parti indesiderate da un testo o un file. È utile per manipolare dati e fare modifiche precise in modo rapido ed efficiente.

# Come fare:

Ecco un esempio di come cancellare un carattere da una stringa utilizzando il Fish Shell:

```
set my_string "Hello World!"
echo $my_string
set my_string (string replace $my_string "!" "")
echo $my_string
```
Output: 
```
Hello World!
Hello World

# Deep Dive:

La cancellazione di caratteri è stata utilizzata fin dall'inizio della programmazione. In precedenza, questa operazione veniva effettuata principalmente con i comandi “sed” e “tr”. Tuttavia, grazie alla sintassi intuitiva e alla robustezza di Fish Shell, molti programmatori preferiscono utilizzare il Fish Shell per questa operazione.

Esistono anche altre alternative, come l'utilizzo di espressioni regolari e l'utilizzo delle funzioni “find” e “replace”. Tuttavia, nel complesso, utilizzare il Fish Shell per la cancellazione di caratteri corrispondenti a uno schema è un modo semplice e veloce per ottenere i risultati desiderati.

Inoltre, è possibile personalizzare la cancellazione di caratteri utilizzando le opzioni disponibili nella funzione “string replace”. Ad esempio, è possibile specificare un numero massimo di sostituzioni desiderate o specificare se si desidera cancellare in modo insensibile alle maiuscole o alle minuscole.

# Vedi anche:

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/cmds/replace.html)
- [French Fry's Guide to Fish Shell](https://read.thenvpl.com/fish-guide#replacing-substrings)
- [Esempi di utilizzo di string replace](https://gist.github.com/JorgeBucaran/fa0eea9ab69c13e9de177a8a74a0a941)