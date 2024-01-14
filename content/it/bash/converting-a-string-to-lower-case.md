---
title:    "Bash: Convertire una stringa in minuscolo"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere molto utile quando si lavora con dati di input, come ad esempio in uno script di Bash. Ciò consente di rendere uniforme il formato dei dati e semplifica la loro manipolazione.

## Come Fare

Ecco un esempio di come convertire una stringa in minuscolo utilizzando il comando `tr` in Bash:

```Bash
stringa="CIAO AMICI!"
echo "$stringa" | tr '[:upper:]' '[:lower:]'
```

L'output di questo codice sarà `ciao amici!`, poiché il comando `tr` sostituisce tutti i caratteri maiuscoli con quelli minuscoli.

## Approfondiamo

Per comprendere meglio come funziona la conversione di una stringa in minuscolo, è importante conoscere l'utilizzo dei caratteri speciali `[:upper:]` e `[:lower:]` in Bash. Questi caratteri indicano rispettivamente tutti i caratteri maiuscoli e tutti i caratteri minuscoli nell'alfabeto. Usando il comando `tr` possiamo sostituire uno o più di questi caratteri con un altro carattere o con uno spazio bianco.

Un altro modo per convertire una stringa in minuscolo è utilizzare il comando `tr` con le opzioni `-d` e `-s`. Ad esempio:

```Bash
stringa="CIAO AMICI!"
echo "$stringa" | tr -d '[:upper:]' | tr -s '[:lower:]'
```

In questo caso, il primo comando `tr` elimina tutti i caratteri maiuscoli dalla stringa, mentre il secondo comando `tr` sostituisce gli spazi multipli con uno spazio singolo, rendendo così la stringa completamente in minuscolo.

## Vedi Anche

- Documentazione ufficiale di Bash: https://www.gnu.org/software/bash/
- Tutorial su come usare il comando `tr`: https://linuxize.com/post/tr-command-in-linux/
- Altro esempio di conversione di stringhe in minuscolo: https://www.baeldung.com/linux/convert-string-lowercase