---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

La concatenazione delle stringhe è l'operazione che unisce due o più stringhe in una sola. I programmatori la usano per creare dinamicamente le stringhe, per esempio, creando percorsi di file o messaggi per l'utente.

## Come si fa:

Nell'esempio seguente, vediamo come concatenare stringhe in Fish Shell usando l'operatore `string join`.

```Fish Shell
# Definiamo due variabili
set nome "Mario"
set cognome "Rossi"

# Concateniamo le stringhe
set nome_completo (string join " " $nome $cognome)

# Stampa il risultato
echo $nome_completo
```

Output:
```
Mario Rossi
```

## Approfondimento

Prima di Fish Shell, le shell script avevano modi differenti per concatenare le stringhe. Alcune usavano l'operatore '+', altre usavano l'operatore '.'. Fish Shell ha introdotto il comando `string` per unificare le operazioni sulle stringhe in un unico comando. Sebbene ci siano alternative, come `set -x`, la maggior parte dei programmatori preferisce `string join` per la sua leggibilità.

In termini di implementazione, quando si esegue `string join`, Fish Shell concatena le stringhe in memoria prima di assegnarle alla variabile di destinazione. Questo è più efficiente, ma può utilizzare più memoria se le stringhe sono molto lunghe.

## Per Saperne di Più

Per saperne di più sulla concatenazione di stringhe in Fish Shell, consulta le seguenti risorse:

- Documentazione ufficiale di Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutorial su string join: [https://fishshell.com/docs/current/cmds/string-join.html](https://fishshell.com/docs/current/cmds/string-join.html)
- Discussione su StackOverflow su come concatenare stringhe in Fish Shell: [https://stackoverflow.com/questions/21192760/concatenate-strings-in-fish-shell](https://stackoverflow.com/questions/21192760/concatenate-strings-in-fish-shell)