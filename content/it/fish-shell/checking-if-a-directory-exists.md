---
title:                "Verificare l'esistenza di una cartella"
html_title:           "Fish Shell: Verificare l'esistenza di una cartella"
simple_title:         "Verificare l'esistenza di una cartella"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti aver bisogno di verificare se una directory esiste. Ad esempio, quando stai scrivendo uno script che deve fare riferimento a una directory specifica o quando vuoi evitare errori durante l'esecuzione di un comando che richiede una directory esistente.

## Come fare

Per verificare se una directory esiste utilizzando Fish Shell, puoi utilizzare il comando `test` seguito dalla flag `-d` (che significa "directory") e il percorso della directory che desideri verificare. Ad esempio:

```Fish Shell
test -d ~/Documenti
```

Se la directory esiste, il comando restituirà il valore `true`, altrimenti restituirà il valore `false`.

Puoi anche utilizzare il comando `test` insieme alla condizione `if` per eseguire delle azioni specifiche in base al risultato della verifica. Ad esempio:

```Fish Shell
if test -d ~/Documenti
	echo "La directory esiste"
else
	echo "La directory non esiste"
end
```

## Approfondimento

Il comando `test` viene utilizzato per verificare una varietà di condizioni, non solo l'esistenza di una directory. Puoi trovare ulteriori informazioni su questo comando e sulle altre condizioni utilizzabili sul sito ufficiale di Fish Shell.

## Vedi anche

- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/
- Guida pratica alla programmazione con Fish Shell: https://github.com/jorgebucaran/fisher
- Articolo su come gestire le directory in Fish Shell: https://medium.com/@carlosacant/basics-of-working-with-directories-in-fish-shell-676e47617957