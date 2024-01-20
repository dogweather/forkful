---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
La ricerca e la sostituzione del testo sono operazioni comuni in programmazione. Si tratta di trovare una stringa particolare in un certo contesto (un file, un input utente, etc.) e cambiare quella stringa con un’altra. Questo è utile quando vogliamo modificare dei dati in blocco o correggere degli errori.

## Come Fare:
Per la ricerca e la sostituzione del testo in Bash, utilizzeremo `sed`, un'utility molto versatile. Ecco un semplice esempio:
```Bash
echo "Ciao, mondo!" | sed 's/mondo/gente/'
```
Il risultato sarà "`Ciao, gente!`" dal momento che `sed` ha cercato la parola "mondo" e l'ha sostituita con "gente".

## Approfondimenti:
`sed` è uno strumento storico del sistema Unix, nato negli anni '70. Nonostante la sua età, rimane uno degli strumenti più potenti per manipolare il testo nella riga di comando.

Come alternativa a `sed`, abbiamo `awk` che anche lui può effettuare la ricerca e sostituzione del testo, ma è un po' più complicato da utilizzare. Un altro strumento è `grep` che però è utilizzato più per cercare che per sostituire.

La funzione `s/ricerca/sostituzione/` in `sed` è piuttosto semplice da capire. La `s` sta per sostituzione, `/ricerca/` è il testo che vogliamo trovare e `/sostituzione/` è il testo con cui vogliamo sostituirlo.

## Vedi Anche:
Per un approfondimento sulla ricerca e sostituzione del testo in Bash:

* [Sed - An Introduction and Tutorial by Bruce Barnett](http://www.grymoire.com/Unix/Sed.html)
* [How to Use Awk and Grep Commands in Shell Scripting](https://www.educba.com/awk-command-in-unix/)
* [Using Bash's built-in commands (from The Linux Command Line, 2nd Edition)](https://www.learnshell.org/)