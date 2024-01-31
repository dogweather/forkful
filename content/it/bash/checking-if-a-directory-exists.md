---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
simple_title:         "Verifica dell'esistenza di una directory"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Cos'è e Perché?)
Controllare l'esistenza di una directory ci permette di verificare se un particolare percorso nel file system è presente. I programmatori fanno questo per prevenire errori, configurare l'ambiente di lavoro oppure per condizionare l'esecuzione di script a seconda della presenza di specifiche cartelle.

## How to (Come fare):
```Bash
# Usare '-d' per verificare l'esistenza di una directory
if [ -d "/percorso/alla/directory" ]; then
  echo "La directory esiste."
else
  echo "La directory non esiste."
fi

# Uscita di esempio se la directory esiste
La directory esiste.

# Uscita di esempio se la directory non esiste
La directory non esiste.
```

## Deep Dive (Approfondimento):
Il comando `[ -d "/percorso/alla/directory" ]` è uno dei modi più comuni per controllare l'esistenza di una directory in Bash. La flag `-d` restituisce vero (`true`) se il percorso specificato è una directory. È essenziale nei primi script Unix e rimane un approccio standard ancora oggi.

Come alternativa, puoi usare `[[ -d /path ]]` con doppie parentesi per una valutazione più moderna che offre un'estensione delle funzionalità, come il globbing e la sostituzione delle parole.

Per gli scenari complessi, i programmatori possono considerare il comando `find` o `test` (alias `[`), che offre molteplici operazioni di test.

In alcuni casi, potrebbe essere importante controllare anche i permessi della directory per leggere, scrivere o eseguire file al suo interno.

## See Also (Vedi Anche):
- [Bash man page](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
- [Stack Overflow - Check if a directory exists in a shell script](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)
