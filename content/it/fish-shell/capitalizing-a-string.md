---
title:                "Fish Shell: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
  
Lo sviluppo di script e codice è un compito comune per programmatori e sviluppatori. A volte, è necessario manipolare o formattare i dati all'interno del codice per renderlo più leggibile o facilmente identificabile. Una di queste operazioni comuni è la capitalizzazione di una stringa. In questo articolo, esploreremo come farlo utilizzando Fish Shell.

## Come Fare

Per capitalizzare una stringa utilizzando Fish Shell, segue un esempio di codice:

````Fish Shell
set str "ciao mondo"
echo $str | tr '[:lower:]' '[:upper:]'
````
Questo codice userà il comando "set" per definire la variabile "str" come "ciao mondo". Successivamente, verrà utilizzato il comando "echo" e il comando "tr" per convertire tutti i caratteri in minuscolo in maiuscolo. Il risultato sarà "CIAO MONDO". 

## Approfondimento

C'è un po' più di complessità dietro la capitalizzazione di una stringa in Fish Shell. Il comando "tr" viene utilizzato per la traduzione dei caratteri secondo il set di caratteri specificato nella seconda parte del comando. In questo caso, il set di caratteri è '[:lower:]' e '[:upper:]', che rappresenta tutti i caratteri minuscoli e maiuscoli. Potrebbero esserci altri set di caratteri disponibili, quindi assicurarsi di controllare la documentazione per saperne di più.

## Vedi Anche

- Documentazione Fish Shell su "set": https://fishshell.com/docs/current/cmds/set.html
- Documentazione Fish Shell su "tr": https://fishshell.com/docs/current/cmds/tr.html