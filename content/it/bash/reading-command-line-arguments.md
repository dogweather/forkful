---
title:                "Bash: Lettura degli argomenti della riga di comando"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando è un fondamentale abilità nel mondo della programmazione Bash. Sapere come utilizzare gli argomenti della riga di comando può semplificare e rendere più efficiente il processo di scrittura di script.

## Come fare

La lettura degli argomenti della riga di comando avviene attraverso l'utilizzo di variabili speciali predefinite in Bash. La variabile "$1" rappresenta il primo argomento inserito nella riga di comando, "$2" rappresenta il secondo e così via. Ecco un esempio di codice che stampa il terzo argomento inserito nella riga di comando:

```Bash
echo "Il terzo argomento è: $3"
```

Ecco un possibile output di questo codice:

```Bash
$ bash script.sh arg1 arg2 arg3
Il terzo argomento è: arg3
```

È importante notare che gli argomenti della riga di comando sono separati da spazi e gli spazi possono essere gestiti in modo diverso in base alle opzioni utilizzate durante l'esecuzione dello script. Ad esempio, utilizzando le doppie virgolette, possiamo leggere un argomento che contiene spazi al suo interno come un unico valore:

```Bash
read -ra args <<< "$@"
```

Questa opzione creerà un array, chiamato "args", che conterrà tutti gli argomenti della riga di comando.

## Approfondimento

Oltre alle variabili speciali, esistono anche altre tecniche per leggere gli argomenti della riga di comando in Bash, come l'utilizzo delle opzioni "getopts" e "shift". Queste opzioni consentono di gestire gli argomenti in modo più dinamico e sofisticato.

Inoltre, è importante conoscere le differenze tra gli argomenti posizionali e le opzioni a riga di comando (come "-h" o "--help"). Gli argomenti posizionali sono sempre obbligatori, mentre le opzioni possono essere facoltative e spesso vengono utilizzate per fornire informazioni aggiuntive all'esecuzione dello script.

## Vedi anche

- [La guida ufficiale di Bash su come leggere gli argomenti della riga di comando](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Un tutorial dettagliato su come utilizzare le opzioni "getopts" e "shift" per leggere gli argomenti della riga di comando](https://linuxhint.com/bash_parameter_parsing/)
- [Un articolo che spiega le differenze tra gli argomenti posizionali e le opzioni a riga di comando in Bash](https://linuxacademy.com/blog/linux/shell-scripting-explaining-parameters/)