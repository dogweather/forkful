---
title:                "Fish Shell: Convertire una stringa in caratteri minuscoli"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perchè

Molte volte, nel processo di sviluppo di un programma, ci troviamo a dover manipolare delle stringhe di testo. Una delle operazioni più comuni è quella di convertire una stringa in lettere minuscole. Questo può essere utile per confrontare stringhe senza tenere conto delle maiuscole e delle minuscole o per uniformare il formato di un testo.

## Come Fare

Per convertire una stringa in lettere minuscole in Fish Shell è possibile utilizzare il comando `string tolower`. Basta inserire la stringa da convertire tra parentesi tonde dopo il comando. Ecco un esempio:

```Fish Shell
string tolower "Ciao Mondo!"
```

Questo produrrà l'output "ciao mondo!". Nota che le lettere accentate rimarranno invariare.

Un'altra opzione per convertire una stringa in lettere minuscole è utilizzare la funzione built-in `lowercase`. Quindi, il comando diventerebbe:

```Fish Shell
lowercase "Ciao Mondo!"
```

Il risultato è lo stesso dell'esempio precedente.

## Deep Dive

In realtà, la conversione delle stringhe in lettere minuscole avviene utilizzando lo standard Unicode. Questo significa che il comando `string tolower` o la funzione `lowercase` non funzioneranno correttamente se si opera su un sistema con una codifica dei caratteri diversa da Unicode.

Inoltre, con la funzione `lowercase`, è possibile specificare una lingua opzionale per preservare i caratteri accentati. Ad esempio, `lowercase -L it_IT "Ciao Mondo!"` produrrà come output "ciao mondo!" invece di "ciao mondo!".

## Vedi Anche

- [Documentazione Fish Shell di `string tolower`](https://fishshell.com/docs/current/cmds/string.html#tolower)
- [Documentazione Fish Shell di `lowercase`](https://fishshell.com/docs/current/cmds/lowercase.html)