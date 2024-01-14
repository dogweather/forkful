---
title:                "Bash: Cancellazione dei caratteri che corrispondono a un modello"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Eliminare caratteri che corrispondono ad un determinato pattern può essere utile per sanificare i dati di un file, per esempio, rimuovendo tutte le occorrenze di caratteri speciali o spazi vuoti.

## Come Fare

Per eliminare caratteri che corrispondono ad un pattern in Bash, è possibile utilizzare il comando "sed". Ad esempio, per rimuovere tutte le occorrenze di spazi vuoti da un file di testo e salvare la versione modificata in un nuovo file, si può utilizzare il seguente codice:

```Bash
sed 's/ //g' file_originale.txt > file_modificato.txt
```

Quello che fa il comando "sed" è sostituire tutti i caratteri che corrispondono al pattern indicato tra le barre "//" con una stringa vuota (nel nostro caso, non inseriamo nulla dopo la seconda barra). Il parametro "g" indica di sostituire tutti i caratteri del pattern, non solo la prima occorrenza.

## Deep Dive

Il comando "sed" è molto versatile e può essere utilizzato per svolgere una varietà di operazioni di editing di testo. Per esempio, oltre alla sostituzione, è possibile utilizzarlo per inserire nuove linee di testo, eliminare linee specifiche, o persino sostituire il contenuto in base ad un'espressione regolare. È possibile consultare la documentazione ufficiale di "sed" per ulteriori dettagli e opzioni.

## Vedi Anche

- Documentazione ufficiale di sed: https://www.gnu.org/software/sed/manual/sed.html
- Un tutorial su come utilizzare sed: https://www.baeldung.com/linux/sed-command