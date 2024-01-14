---
title:                "Fish Shell: Converting una stringa in minuscolo"
simple_title:         "Converting una stringa in minuscolo"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante la scrittura di script, ci si imbatte nella necessità di trasformare una stringa in minuscolo per eseguire confronti o elaborazioni. Fortunatamente, Fish Shell ci offre una semplice soluzione per questo problema.

## Come fare

Per trasformare una stringa in minuscolo, è possibile utilizzare il comando `string tolower` seguito dalla stringa da convertire. Vediamo un esempio:

```Fish Shell
string tolower "Ciao a Tutti!"
```

Output: `ciao a tutti!`

Come si può osservare, il comando ha semplicemente convertito tutti i caratteri della stringa in minuscolo. Se si vuole salvare il risultato della conversione in una variabile, è necessario utilizzare la sintassi `set -l`. Ad esempio:

```Fish Shell
set -l lower_case (string tolower "Questa stringa Verrà CONVERTITA")
echo $lower_case
```

Output: `questa stringa verrà convertita`

È anche possibile convertire una stringa in minuscolo all'interno di una funzione. Basterà utilizzare il comando `string tolower` all'interno della funzione per ottenere il risultato desiderato.

## Approfondimento

Il comando `string tolower` utilizza le regole standard di conversione delle lettere in base al locale corrente del sistema operativo. Questo significa che il risultato della conversione può differire a seconda della lingua e delle impostazioni del sistema in cui viene eseguita.

Inoltre, è possibile utilizzare il comando `string toupper` per convertire una stringa in maiuscolo, e il comando `string capitalize` per convertire la prima lettera di ogni parola in maiuscolo.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.it.html)
- [Tutorial sulle stringhe in Fish Shell](https://fishshell.com/docs/current/tutorial.html#strings)
- [Altre funzionalità utili di Fish Shell](https://fishshell.com/docs/current/commands.html)