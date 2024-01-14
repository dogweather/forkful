---
title:    "Bash: Convertire una stringa in minuscolo."
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Spesso in programmazione ci troviamo ad avere a che fare con stringhe di testo. E se abbiamo bisogno di convertire una stringa in minuscolo? Il motivo potrebbe essere quello di uniformare il testo o di facilitare la ricerca e il confronto di stringhe.

## Come Fare

Per convertire una stringa in minuscolo in Bash, possiamo utilizzare il comando `tr`. Questo comando ci permette di trasformare un carattere in un altro carattere o, nel nostro caso, di convertire una stringa in minuscolo.

Ecco un esempio di codice:

```Bash
str="HELLO WORLD"
lowercase=$(echo "$str" | tr '[:upper:]' '[:lower:]')
echo "$lowercase"
```

L'output di questo codice sarà `hello world`, con la stringa convertita in minuscolo.

## Approfondimento

Il comando `tr` è molto utile anche per sostituire un carattere con un altro o per eliminare determinati caratteri da una stringa. Possiamo utilizzare anche l'operatore di redirect `>` per salvare il risultato della conversione in un nuovo file.

Un'altra opzione per convertire una stringa in minuscolo può essere l'utilizzo del comando `sed`. Questo comando ci permette di sostituire i caratteri di una stringa con altri caratteri. Utilizzando l'opzione `i` possiamo specificare di sostituire solo le lettere maiuscole con le corrispondenti lettere minuscole.

Di seguito un esempio di codice:

```Bash
string="BENVENUTI"
lowercase=$(sed 's/[[:upper:]]*/\L&/g' <<< "$string")
echo "$lowercase"
```

L'output sarà sempre `benvenuti`, con la stringa convertita in minuscolo.

## Vedi Anche

- Documentazione su `tr`: https://linux.die.net/man/1/tr
- Documentazione su `sed`: https://linux.die.net/man/1/sed