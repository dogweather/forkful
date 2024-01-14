---
title:                "Bash: Scrivere su errore standard."
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error è una pratica comune nella programmazione Bash. Quando si scrive uno script, comunicare con l'utente è fondamentale per una corretta interazione. Utilizzando la standard error, possiamo fornire messaggi di errore istantanei e permettere all'utente di comprendere meglio ciò che sta accadendo nel programma.

## Come farlo

Per scrivere a standard error in Bash, utilizzeremo il comando `echo` insieme all'operatore `>&2`. Questo reindirizzerà l'output del comando echo verso il canale di errore standard invece del solito canale di output standard. Ecco un esempio di codice che mostra come utilizzare correttamente il comando `echo` per scrivere a standard error:

```Bash
echo "Ops! Questo è un messaggio di errore" >&2
```

L'output di questo comando verrà inviato a standard error invece di standard output come di norma. Nel caso in cui si desideri specificare un messaggio diverso dall'errore predefinito, è possibile utilizzare il codice di uscita di 2 come indicato di seguito:

```Bash
echo "Messaggio di errore personalizzato" >&2
exit 2
```

Utilizzare il codice di uscita di 2 indicherà all'utente che c'è stato un errore nel programma. Inoltre, è importante notare che è possibile utilizzare qualsiasi comando all'interno della parentesi `(&)` per inviare la sua output a standard error.

## Approfondimento 

Ora che abbiamo visto come scrivere a standard error, è importante capire il suo utilizzo in situazioni più complesse. Quando si lavora con script di grandi dimensioni, è fondamentale tenere sotto controllo gli errori e comunicare con l'utente in modo efficace. È possibile utilizzare la standard error per gestire gli errori in modo più specifico e fornire indicazioni all'utente su come risolverli.

Inoltre, quando si tratta di log di sistema, la standard error è molto utile per identificare rapidamente e correttamente gli errori di un programma. Invece di dover cercare tra l'output standard, è possibile controllare direttamente gli errori su standard error e risolverli più rapidamente.

## Vedi anche

- [Guida completa ai comandi Bash](https://linux.die.net/Bash)
- [Utilizzo di standard error in script Bash](https://www.linuxquestions.org/linux/answers/Programming/Using_the_standard_error_output_in_BASH_scripts) 
- [Articolo su standard input, output e error in Bash](https://www.linuxjournal.com/content/bash-redirections-using-exec)

Grazie per aver letto questo post! Spero che ora tu comprenda l'importanza di scrivere a standard error nei tuoi programmi Bash. Continua a sperimentare e utilizzare questa tecnica per migliorare la tua esperienza di programmazione. Buona fortuna!