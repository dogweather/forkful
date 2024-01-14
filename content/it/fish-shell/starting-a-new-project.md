---
title:    "Fish Shell: Iniziare un nuovo progetto"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché 

Ciao a tutti e benvenuti su questo post dedicato alla programmazione in Fish Shell! Se stai leggendo questo articolo, probabilmente ti stai chiedendo perché dovresti iniziare un nuovo progetto con Fish Shell. Ci sono molti buoni motivi per farlo, ma tra i più rilevanti possiamo citare la sua velocità, la sua potenza e la sua semplicità nell'utilizzare variabili ed eseguire comandi.

## Come fare

Per iniziare un nuovo progetto con Fish Shell, inizia aprendo il tuo terminale e digita il comando `fish`. Questo ti porterà alla shell di Fish, dove potrai iniziare a scrivere il tuo codice. 

```Fish Shell
$ fish
```

Una delle funzionalità più utili di Fish Shell è il completamento automatico: basta premere il tasto `Tab` mentre si scrive un comando o un percorso di file e Fish Shell lo completerà per te. Ad esempio, se vuoi cambiare directory in una cartella chiamata "Documenti", basta scrivere `cd Doc` e premere `Tab` per completare il resto del nome.

## Un'immersione più profonda

Adesso che hai familiarizzato con i comandi di base di Fish Shell, è il momento di fare un tuffo più profondo e imparare alcune tecniche avanzate per avviare un nuovo progetto. Uno dei vantaggi di Fish Shell rispetto a altre shell è la sua gestione dei percorsi di sistema attraverso le variabili. Per esempio, puoi usare la variabile `$PATH` per specificare i percorsi in cui Fish Shell cercherà comandi da eseguire. 

```Fish Shell
set -gx PATH $PATH ~/my_project/bin
```

È possibile anche impostare le proprie variabili personalizzate per semplificare il lavoro con specifici progetti. Per esempio, puoi creare una variabile `$PROJECT_PATH` che punta alla tua cartella principale di progetto e usare questa variabile per spostarti rapidamente al suo interno.

## Vedi anche

- [Sito ufficiale di Fish Shell](https://fishshell.com)
- [Documentazione di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Esempi di codice in Fish Shell](https://github.com/fish-shell/fish-shell/wiki/Built-in-Commands)
- [Impostare le variabili in Fish Shell](https://codereviewvideos.com/blog/set-fish-shell-variables-per-progetto)