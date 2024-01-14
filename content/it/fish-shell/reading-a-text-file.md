---
title:    "Fish Shell: Lettura di un file di testo"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Stai cercando un modo semplice e veloce per leggere un file di testo nella tua riga di comando? Allora sei nel posto giusto! In questo post imparerai come utilizzare il Fish Shell per leggere e visualizzare il contenuto di un file di testo.

## Come fare

Per prima cosa, assicurati di avere Fish Shell installato sul tuo sistema. Una volta fatto ciò, apri il tuo terminale e segui questi passaggi:

1. Utilizzare il comando `cd` per navigare nella cartella in cui si trova il file di testo che desideri leggere.
2. Assicurati che il file di testo sia presente nella cartella eseguendo il comando `ls`.
3. Utilizza il comando `cat` seguito dal nome del file per visualizzare il contenuto del file nella tua riga di comando.

Ecco un esempio di codice:

```
cd Documents/
ls
cat file.txt
```

Una volta eseguiti questi passaggi, dovresti essere in grado di vedere il contenuto del tuo file di testo stampato nella tua console.

## Deep Dive

Oltre al comando `cat`, c'è un'altra opzione per leggere un file di testo nel Fish Shell. Puoi utilizzare il comando `less` che ti permette di scorrere il contenuto del file di testo pagina per pagina. Per uscire dalla modalità `less`, premi la lettera `q` sulla tua tastiera.

Un'altra opzione è utilizzare il comando `head` per visualizzare solo le prime righe del file di testo o `tail` per visualizzare solo le ultime righe. Puoi specificare il numero di righe da visualizzare utilizzando l'opzione `-n`, ad esempio `head -n 10 file.txt` per visualizzare solo le prime 10 righe.

Una funzionalità utile che offre il Fish Shell è la possibilità di utilizzare wildcards per leggere più file di testo contemporaneamente. Ad esempio, se hai una serie di file con lo stesso prefisso (es. `file1.txt`, `file2.txt`, ecc.) puoi utilizzare `cat file*.txt` per visualizzare il contenuto di tutti i file con il prefisso specificato.

## Vedi anche

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial per iniziare con Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell-in-ubuntu-16-04)
- [Comandi essenziali di Fish Shell](https://linuxhint.com/basic_fish_shell_commands/)
- [Guida di riferimento di Fish Shell](https://devhints.io/fish)

Grazie per aver letto questo post sulla lettura di file di testo nel Fish Shell. Speriamo che ti sia stato utile e non vediamo l'ora di vederti utilizzare questi comandi nella tua prossima sessione di codifica!