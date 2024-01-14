---
title:                "Elm: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Perché
Spesso quando si sviluppa un programma, si ha la necessità di conoscere la lunghezza di una determinata stringa. Con Elm, questa operazione è molto semplice grazie alle funzioni built-in fornite dal linguaggio. In questo articolo, vedremo come trovare la lunghezza di una stringa in Elm e perché questa operazione può essere utile nel nostro codice.

# Come Fare
Per trovare la lunghezza di una stringa in Elm, possiamo utilizzare la funzione `String.length` che ci restituisce il numero di caratteri presenti nella stringa. Di seguito un esempio di codice che utilizza questa funzione:

```Elm
nome = "Marco"
lunghezza = String.length nome
```

In questo caso, la variabile `lunghezza` avrà il valore 5, corrispondente alla lunghezza della stringa "Marco". Possiamo anche utilizzare questa funzione per controllare la validità di una password, assicurandoci che abbia una lunghezza minima di caratteri. Di seguito un esempio di codice che utilizza questa idea:

```Elm
password = "elm123"
if String.length password >= 6 then
    -- codice per la gestione della password valida
else
    -- codice per la gestione della password non valida
```

Come possiamo vedere, la funzione `String.length` può essere molto utile per gestire le stringhe nel nostro codice.

# Approfondimento
Oltre alla funzione `String.length`, possiamo anche utilizzare il tipo di dato `String` e accedere alla proprietà `length` per ottenere la lunghezza di una stringa. Ad esempio:

```Elm
nome = "Marco"
lunghezza = nome.length
```

Entrambe le opzioni sono valide, ma la funzione built-in `String.length` è generalmente più performante poiché fa parte del modulo `Basics` già importato automaticamente in ogni file Elm.

# Vedi Anche
- Documentazione ufficiale di Elm sulle stringhe: https://package.elm-lang.org/packages/elm/core/latest/String
- Esempi di codice per trovare la lunghezza di una stringa in Elm: https://elmprogramming.com/finding-the-length-of-a-string-in-elm.html