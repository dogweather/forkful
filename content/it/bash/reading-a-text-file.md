---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

La lettura di un file di testo è l'azione di estrarre dati testuali da un file. Lo facciamo nel programming per manovrare i dati, per trasformare o analizzare le informazioni contenute nel file.

## Come fare:

Per leggere un file di testo in Bash, possiamo utilizzare il comando `cat`. Ecco un esempio semplice:

```Bash
$ cat my-file.txt
```

Questo comando stampa il contenuto del file `my-file.txt` sulla console.

Inoltre, potremmo voler leggere un file linea per linea. Per fare ciò, utilizziamo un ciclo `while`:

```Bash
$ while read line
> do
>   echo $line
> done < my-file.txt
```

Questo script legge `my-file.txt` linea per linea e stampa ogni linea sulla console.

## Approfondimento

La lettura dei file di testo è una funzione fondamentale in programmazione e ci sono molte strade per farlo. In Bash, utilizziamo prevalentemente le funzioni "cat", "while read line" o "read". La lettura dei file è presente fin dagli inizi dell'epoca Unix, con la shell Bourne fornita con Unix V7.

Una alternativa a Bash per la lettura dei file è l'uso di linguaggi più avanzati come Python o Perl, che possono offrire un controllo più dettagliato.

Tuttavia, tenete presente che la lettura di un file con le funzioni Bash è più adatta a file di dimensioni moderata. Per file molto grandi, altri strumenti come `awk` o `sed` possono essere più efficienti.

## Vedere Anche:

- [Ubuntu Manpage - bash](http://manpages.ubuntu.com/manpages/focal/it/man1/bash.1.html)
- [AWK Manpage](https://www.gnu.org/software/gawk/manual/gawk.html)
- [SED Manpage](https://www.gnu.org/software/sed/manual/sed.html)