---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Conoscere la lunghezza di una stringa significa capire il numero di caratteri in essa. I programmatori lo fanno per gestire meglio l'input, fare il debug più facilmente o per manipolare i dati in modo più efficace.

## Come si fa:

Ecco come trovare la lunghezza di una stringa in Bash utilizzando il comando "string length".

```Bash
s="Hello, World!"  
lunghezza=${#s}  
echo $lunghezza
```

L'output sarà "13", il numero dei caratteri presenti in "Hello, World!".

## Approfondimento

Bash, o il "Bourne Again Shell", è stato originariamente scritto da Brian Fox per il progetto GNU alla fine degli anni '80. Il comando per trovare la lunghezza di una stringa è stato introdotto per offrire ai programmatori un modo facile e veloce per gestire le stringhe.

Esistono alternative per trovare la lunghezza di una stringa in Bash, ad esempio, usando il comando `expr` o `awk`. Tuttavia, usando `${#s}`, possiamo ottenere la lunghezza di una stringa più direttamente e rapidamente.

Un dettaglio importante da menzionare è che il comando `${#s}` conta i byte, non i caratteri. Questo significa che non gestisce correttamente le stringhe con caratteri multibyte, come gli emoji.

## Vedi anche

Se desideri ulteriori informazioni, consulta le seguenti risorse:

1. Pagina del manuale di Bash: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)

2. Guida completa di Bash Scripting: [https://linuxconfig.org/bash-scripting-tutorial](https://linuxconfig.org/bash-scripting-tutorial)

3. Introduzione alla programmazione di Shell: [https://www.tldp.org/LDP/Bash-Beginners-Guide/html/](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)