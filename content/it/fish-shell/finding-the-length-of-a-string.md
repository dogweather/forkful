---
title:                "Trova la lunghezza di una stringa"
html_title:           "Fish Shell: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Il comando "string" in Fish Shell è utilizzato per ottenere la lunghezza di una stringa di testo. Questo è utile per i programmatori poiché consente loro di determinare la dimensione di una stringa per fini di manipolazione dei dati o di convalida dei dati inseriti dall'utente.

## Come fare:

Fish Shell rende facile trovare la lunghezza di una stringa di testo. Basta digitare il seguente comando in un terminale:

```
string length "Ciao!"
```

Questo restituirà un valore numerico che rappresenta la lunghezza della stringa "Ciao!". È anche possibile utilizzare questo comando per ottenere la lunghezza di una variabile contenente una stringa di testo:

```
set mystring "Hello World!"
string length $mystring
```

Questo restituirà lo stesso valore numerico come il comando precedente. Inoltre, è possibile combinare il comando "string length" con altri comandi di Fish Shell, come ad esempio "echo", per stampare direttamente la lunghezza della stringa:

```
echo La lunghezza della stringa è (string length "Ciao!")
```

Questo restituirà una frase come "La lunghezza della stringa è 5".

## Approfondimenti:

Il comando "string length" è stato introdotto in Fish Shell nella versione 2.0, che è stata rilasciata nel 2014. Prima di questo, i programmatori dovevano utilizzare comandi più verbosi per ottenere la lunghezza di una stringa, come ad esempio "echo (echo "Hello World" | wc -c)".

In alternativa, altri linguaggi di shell come Bash o Zsh hanno il comando nativo "length" per trovare la lunghezza di una stringa. Tuttavia, Fish Shell ha un'implementazione più semplice e intuitiva tramite il comando "string length".

## Vedi anche:

- Documentazione ufficiale di Fish Shell per il comando "string": https://fishshell.com/docs/current/cmds/string.html#length
- Tutorial su Fish Shell: https://fishshell.com/docs/current/tutorial.html