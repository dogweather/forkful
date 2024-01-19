---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Trova la lunghezza di una stringa è un'operazione che ritorna il numero di caratteri presenti in una determinata stringa. Questo è fondamentale per vari compiti di programmazione, come il conteggio delle parole, la verifica delle password e mille altri usi.

## Come si fa:

Ecco un esempio semplice di come si può trovare la lunghezza di una stringa in Fish Shell.

```Fish Shell
set stringa "Ciao, mondo"
echo (string length $stringa)
```

L'output di questo codice sarà "11" - il numero di caratteri nella stringa "Ciao, mondo".

## Approfondimenti

La funzione `string length` in Fish Shell ha una storia interessante. Nelle prime versioni, non esisteva una funzione incorporata per trovare la lunghezza di una stringa. Ma con il tempo, gli sviluppatori hanno riconosciuto l'importanza di questa operazione e hanno introdotto `string length`.

Oltre a `string length`, ci sono anche altre modalità per trovare la lunghezza di una stringa in Fish Shell. Ad esempio, si può usare la funzione `wc`, che conta il numero di linee, parole o byte:

```Fish Shell
echo -n $stringa | wc -m
```
Notate che `-m` indica a `wc` di contare i caratteri. 

La funzione `string length` di Fish Shell utilizza la funzione C `wcslen` per determinare la lunghezza di una stringa, che è molto più efficiente rispetto a un semplice ciclo di conteggio.

## Vedi Anche:

Per saperne di più sulle operazioni delle stringhe in Fish Shell, consulta i seguenti link:

[Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/3.1/commands.html#string)

[Una guida ai comandi di stringa in Fish Shell](https://fishshell.com/docs/3.1/tutorial.html#tut_strings)

[Una discussione sulle funzioni delle stringhe in StackOverflow](https://stackoverflow.com/questions/tagged/fish)