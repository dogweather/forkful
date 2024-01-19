---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che Cosa & Perchè?

La cancellazione di caratteri corrispondenti a un determinato pattern è un'operazione comune nella programmazione. È utile quando abbiamo bisogno di manipolare stringhe o dati, come rimuovere spazi inutili o formattare l'input utente.

## Come Fare:

Vediamo come fare in Fish Shell. Sia che abbiamo una stringa "Ciao, mondo!". Vogliamo rimuovere tutte le virgole.

```Fish Shell
set stringa "Ciao, mondo!"
echo $stringa | string replace -r "," ""
```

E l'output sarà il seguente.

```Fish Shell 
Ciao mondo!
```

In caso contrario, se vogliamo rimuovere tutte le lettere 'o':

```Fish Shell
echo $stringa | string replace -r "o" ""
```

E l'output sarà il seguente.

```Fish Shell 
Cia, mnd!
```

## Approfondimento

Fish, acronimo di friendly interactive shell, è una shell per UNIX che è intenzionalmente non conforme alle tradizionali shell Posix/Bourne. Fish fornisce potenti caratteristiche orientate all'utente come la sintassi semplice e coerente, l'auto suggerimento dei comandi e una vasta gamma di funzionalità integrate come `string replace` per manipolare le stringhe.

Ci sono molteplici alternative per eliminare i caratteri corrispondenti a un modello: potresti usare `awk`, `sed`, o `tr` in un'altra shell come bash, ma fish shell rende tutto intuitivo e facile da usare.

L'implementazione di fish shell per la cancellazione di caratteri utilizza un motore di espressioni regolari per abbinare i pattern. Se ti senti avventuroso, puoi dare un'occhiata al codice sorgente su GitHub.

## Link Utili

Ecco alcuni link utili per ulteriori informazioni:

1. [Fish Shell Command Documentation](https://fishshell.com/docs/current/commands.html)
2. [Fish Github Source Code](https://github.com/fish-shell/fish-shell)
3. [Learn more about Regular Expressions](https://www.regular-expressions.info/tutorial.html)