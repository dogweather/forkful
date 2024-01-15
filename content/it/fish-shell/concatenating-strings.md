---
title:                "Concatenando stringhe"
html_title:           "Fish Shell: Concatenando stringhe"
simple_title:         "Concatenando stringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Hai mai desiderato creare una stringa unendo più pezzi di testo? Con la shell Fish, puoi farlo facilmente usando il comando di concatenazione delle stringhe. Questo può essere utile quando si vuole creare dinamicamente dei comandi o visualizzare messaggi personalizzati.

## Come fare
Per concatenare le stringhe in Fish Shell, usa il simbolo `+` come operatore di concatenazione. Ad esempio, se vogliamo creare una stringa composta dal nostro nome e cognome, possiamo usare il seguente comando:

```Fish Shell
set nome "Mario"
set cognome "Rossi"
echo $nome + " " + $cognome
```

L'output sarà: `Mario Rossi`.

Se abbiamo bisogno di convertire variabili in stringhe per poterle concatenare, possiamo usare il comando `string` prima delle variabili. Ad esempio:

```Fish Shell
set num1 10
set num2 5
echo (string $num1) + (string $num2)
```

L'output sarà: `105`. Nota che abbiamo utilizzato le parentesi tonde `()` per indicare la precedenza degli operatori, in modo che il comando `string` venga eseguito prima della concatenazione delle stringhe.

## Approfondimento
La concatenazione delle stringhe in Fish Shell è possibile grazie all'utilizzo delle variabili, insieme all'operatore `+`. Si noti che le variabili possono anche essere concatenare senza l'utilizzo del comando `string`, ma in questo caso verrà mostrato un avviso in cui viene suggerito di utilizzare il comando.

È importante anche notare che le variabili che rappresentano numeri non possono essere concatenate con le stringhe, a meno che non vengano prima convertite in stringhe tramite il comando `string`.

## Vedi anche
- [Documentazione ufficiale su Fish Shell](https://fishshell.com/docs/current/)
- [Articolo su come gestire le variabili in Fish Shell](https://dev.to/anandsinha07/working-with-variables-in-fish-shell-406k)