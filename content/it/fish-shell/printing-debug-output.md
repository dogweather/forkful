---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La stampa di debug è un processo nel quale il programmatore stampa i dati delle variabili durante l'esecuzione del programma. È utilizzato per individuare, tracciare e correggere i bug.
 
## Come si fa:

Nel Fish Shell, potete utilizzare il comando `echo` per stampare i dati in console. Supponiamo di voler vedere il valore di una variabile `x`:

```Fish Shell
set x 5
echo $x
```

Dà in uscita: 

```
5
```

Se si vuole stampare un messaggio di debug, si può utilizzare la seguente sintassi:

```Fish Shell
echo "Il valore di x è $x"
```

Dà in uscita: 

```
Il valore di x è 5
```

## Approfondimento

Historicamente, la stampa di debug è stata uno dei primi metodi usati per il debugging. Oggigiorno esistono debugger più avanzati, ma la stampa rimane un modo semplice e veloce per il debugging.

Per quanto riguarda le alternative, potreste utilizzare il comando `printf` al posto di `echo`. Offre più opzioni formattative ma è un po' più complesso:

```Fish Shell
printf "Il valore di x è %s\n" $x
```

Potreste inoltre reindirizzare l'output di debug in un file di log anziché stamparlo nel terminale, utilizzando il simbolo `>`:

```Fish Shell
echo "Il valore di x è $x" > output.log
```

Nel Fish Shell, l'implementazione del comando `echo` è abbastanza semplice: prende i suoi argomenti, li converte in stringhe e li stampa sullo standard output.

## Vedi anche

Non fatevi mancare i seguenti link:

- Di più su `echo`: https://fishshell.com/docs/current/cmds/echo.html
- Di più su `printf`: https://fishshell.com/docs/current/cmds/printf.html
- Il debugging in Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_debugging