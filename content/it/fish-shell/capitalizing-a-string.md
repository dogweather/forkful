---
title:    "Fish Shell: Capitalizzare una stringa"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler capitalizzare una stringa nella programmazione Shell di Fish. Ad esempio, potresti voler rendere più leggibile la tua codifica o avere una formattazione consistente nei tuoi output.

## Come fare
Per capitalizzare una stringa in Fish Shell, puoi utilizzare il comando `string capitalize`, seguito dalla stringa che desideri modificare. Ad esempio:

```
string capitalize "ciao a tutti"
```

Questo produrrà l'output "Ciao a tutti". Puoi anche utilizzare il flag `-a` per capitalizzare ogni parola all'interno della stringa, invece di solo la prima lettera. Ad esempio:

```
string capitalize -a "ciao a tutti"
```

Produrrà l'output "Ciao A Tutti".

## Approfondimento
Il comando `string capitalize` in realtà utilizza la funzione `string upcase` di Fish Shell per convertire tutte le lettere in maiuscolo. Tuttavia, se vuoi capitalizzare solo la prima lettera di una stringa, puoi utilizzare direttamente `string upcase -s`.

Inoltre, puoi anche utilizzare l'operazione `~` per concatenare la stringa capitalizzata con il resto della tua codifica. Ad esempio:

```
echo "Ciao"~(string capitalize -a "a tutti")
```

Questo produrrà l'output "Ciao A Tutti".

## Vedi anche
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guida introduttiva a Fish Shell per principianti](https://www.tecmint.com/fishshell-a-command-line-shell-for-linux/)
- [Esempi di codice per fare pratica con Fish Shell](https://gist.github.com/awesomefish/9f6f36d0d2cddfa2d9be)