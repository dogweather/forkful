---
title:                "Bash: Leggere un file di testo"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Se stai cercando un modo semplice e veloce per leggere un file di testo nel tuo programma Bash, sei nel posto giusto! Continua a leggere per scoprire come farlo.

## Come Fare
Per leggere un file di testo in Bash, è possibile utilizzare il comando "cat" seguito dal nome del file che si desidera leggere. Ad esempio:
```Bash
cat file.txt
```
Questo comando stamperà il contenuto del file di testo direttamente sulla tua shell. Se vuoi invece salvare il contenuto del file di testo in una variabile, puoi utilizzare il comando "read":
```Bash
read var < file.txt
```
In questo modo, il contenuto del file di testo verrà salvato nella variabile "var".

## Approfondimento
Esistono molti altri comandi e opzioni per la lettura di file di testo in Bash. Ad esempio, puoi utilizzare il comando "head" per leggere solo le prime righe del file, o il comando "grep" per cercare una specifica parola o frase all'interno del file. È anche possibile utilizzare i comandi "wc" per contare il numero di parole, righe o caratteri all'interno del file. Inoltre, è possibile utilizzare la sintassi "while read" per leggere il file riga per riga all'interno di un ciclo while.

## Vedi Anche
- [La documentazione ufficiale su come leggere un file in Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)

- [Una guida più dettagliata sulla lettura di file di testo in Bash](https://linuxize.com/post/bash-read-file/)