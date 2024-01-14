---
title:    "Fish Shell: Unire stringhe"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione fondamentale nella programmazione e può essere utile per combinare più stringhe in una sola, lavorare con variabili e generare output personalizzati.

## Come Fare

Per concatenare stringhe in Fish Shell, puoi utilizzare l'operatore `+` o la funzione `string join`. Ecco due esempi di codice:

```Fish Shell
set str1 "Ciao"
set str2 "mondo!"
echo $str1$str2
```
```Fish Shell
set arr (start 1 end 5)
string join -s " e " $arr
```

Il primo esempio utilizza l'operatore `+` per unire le due stringhe. L'output sarà `Ciao mondo!`. Nel secondo esempio, la funzione `string join` viene utilizzata per concatenare i valori dell'array `arr` utilizzando lo spazio e come separatore. L'output sarà `1 e 2 e 3 e 4 e 5`.

## Approfondimento

Ci sono molti altri modi per concatenare stringhe in Fish Shell. Ad esempio, puoi utilizzare la funzione `string join -n` per unire i valori di un array senza alcun separatore. Puoi anche utilizzare l'operatore `..` per aggiungere più stringhe insieme. Inoltre, diversi comandi di Fish Shell, come `grep` e `sed`, supportano l'utilizzo di stringhe concatenate per lavorare con i dati in modo più efficiente.

## Vedi Anche

- [Concatenare stringhe using sed](https://fishshell.com/docs/current/cmds/sed.html)
- [Concatenare stringhe using grep](https://fishshell.com/docs/current/cmds/grep.html)
- [Concatenare stringhe using string join](https://fishshell.com/docs/current/cmds/string-join.html)