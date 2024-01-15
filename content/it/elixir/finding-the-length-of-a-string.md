---
title:                "La ricerca della lunghezza di una stringa"
html_title:           "Elixir: La ricerca della lunghezza di una stringa"
simple_title:         "La ricerca della lunghezza di una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Cosa c'è di importante nel trovare la lunghezza di una stringa? Potrebbe sembrare una semplice operazione, ma sapevi che è essenziale in molti aspetti della programmazione? In questo articolo esploreremo come trovare la lunghezza di una stringa utilizzando il linguaggio di programmazione Elixir.

## Come fare
Per trovare la lunghezza di una stringa in Elixir, possiamo utilizzare la funzione `String.length/1`. Questa funzione accetta una stringa come argomento e restituisce il numero di caratteri nella stringa. Vediamo un esempio:

```Elixir
stringa = "Ciao mondo"
String.length(stringa)

# Output: 10
```

In questo esempio, abbiamo creato una variabile `stringa` contenente la frase "Ciao mondo" e poi abbiamo utilizzato la funzione `String.length/1` per trovare la sua lunghezza, che è 10.

K è molto semplice, giusto? Inoltre, possiamo anche utilizzare la sintassi puntata per richiamare la funzione `length` dalla moduli `String`. Vediamo un altro esempio:

```Elixir
stringa = "Questo è un esempio"
String.length(stringa)

# Output: 19
```

## Approfondimento
Oltre alla funzione `String.length/1`, esiste anche un'altra funzione utile per trovare la lunghezza di una stringa in Elixir: `String.codepoints/1`. Questa funzione restituisce una lista delle codepoints (rappresentazioni numeriche dei caratteri) nella stringa. Possiamo utilizzare questa funzione per avere un maggior controllo sui dati contenuti nella stringa.

Vediamo un esempio di questa funzione in azione:

```Elixir
stringa = "casa"
String.codepoints(str)

# Output: [99, 97, 115, 97]
```

Come puoi vedere, la funzione ha restituito una lista di codici numerici che rappresentano i caratteri all'interno della stringa. Possiamo anche utilizzare altre funzioni Elixir, come `Enum.count/1`, per contare il numero di elementi nella lista restituiti dalla funzione `String.codepoints/1`, per ottenere la lunghezza della stringa.

## Vedi anche
- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Enum Module](https://hexdocs.pm/elixir/Enum.html)

In questo articolo, abbiamo scoperto come trovare la lunghezza di una stringa utilizzando Elixir. Ricorda sempre che la lunghezza di una stringa è importante per molte operazioni di manipolazione dei dati e che Elixir offre diverse funzioni per farlo in modo efficiente. Tieni a mente anche la funzione `String.codepoints/1` per un ulteriore controllo dei dati della stringa. Buon coding!