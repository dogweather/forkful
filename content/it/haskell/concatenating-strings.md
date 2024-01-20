---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La concatenazione di stringhe è il processo di unione di due o più stringhe in una sola. È fondamentale per i programmatori costruire testi complessi e dinamici in modo efficiente.

## Come fare:

Vediamo come fare la concatenazione delle stringhe in Haskell. Utilizziamo l'operatore `++`. 

```Haskell
main = do
    let stringa1 = "Ciao "
    let stringa2 = "Mondo!"
    let stringaCompleta = stringa1 ++ stringa2
    putStrLn stringaCompleta
```

E questo sarà l'output.

```
Ciao Mondo!
```

## Approfondimento

(1) Il concetto di concatenazione di stringhe esiste da quando esistono i linguaggi di programmazione. (2) Sebbene l'operatore `++` sia il più comune in Haskell, esistono alternative come la funzione `concat`. `concat` può essere più vantaggioso quando si uniscono molte stringhe insieme.
(3) Nello specifico, quando si utilizza `++`, Haskell crea una nuova stringa, copiando i caratteri delle stringhe originali. Questa può essere una considerazione di prestazione quando si lavora con stringhe più grandi.

## Vedi Anche

2. [Learn You a Haskell](http://learnyouahaskell.com/starting-out#an-intro-to-lists): una risorsa incredibilmente dettagliata per l'apprendimento di Haskell, che include anche informazioni sul concatenamento di stringhe.