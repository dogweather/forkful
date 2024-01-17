---
title:                "Eliminare caratteri che corrispondono a un modello."
html_title:           "Bash: Eliminare caratteri che corrispondono a un modello."
simple_title:         "Eliminare caratteri che corrispondono a un modello."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Eliminare i caratteri che corrispondono a uno schema è un'operazione comune nella programmazione Bash. Ciò significa cercare e rimuovere specifici caratteri all'interno di una stringa o di un file di testo. I programmatori fanno spesso questa operazione per pulire e manipolare i dati o per trovare ed eliminare errori di formattazione.

## Come:

```Bash
# Esempio di comando per eliminare vocali da una stringa
echo "Ciao amici!" | tr -d "aeiou"
# Output: Cm!
```

```Bash
# Esempio di comando per eliminare tutte le linee vuote da un file di testo
sed -i '/^$/d' file.txt
```

## Approfondimento:

In passato, questo tipo di operazione veniva svolto tramite comandi come `grep`, `sed` o `awk`. Tuttavia, oggi Bash ha una funzione integrata per facilitare il processo: `tr -d`. Questo comando può essere utilizzato per eliminare sia caratteri specifici che interi set di caratteri da una stringa. 

Inoltre, esiste anche una forma di sostituzione dei caratteri in Bash utilizzando il comando `tr`. Ad esempio, `tr a-z A-Z` cambierebbe tutti i caratteri minuscoli in maiuscoli.

Inoltre, il comando `sed` ha molte altre funzioni utili per la manipolazione dei file di testo ed è un'alternativa popolare per eliminare caratteri corrispondenti a uno schema. 

## Vedi anche:

- La [documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/html_node/Text-Manipulation.html)
- Un articolo di [How-To Geek](https://www.howtogeek.com/351483/how-to-remove-characters-from-text-strings-with-bash/)
- Una guida su [sed](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux) da DigitalOcean.