---
title:                "Elixir: Trovare la lunghezza di una stringa"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Perché

La ricerca della lunghezza di una stringa è un'operazione comune durante lo sviluppo di un programma in Elixir. Sapere come ottenere la lunghezza di una stringa può semplificare l'analisi e la manipolazione dei dati all'interno del tuo codice.

# Come fare

Un modo semplice per ottenere la lunghezza di una stringa è utilizzare la funzione `String.length/1` di Elixir. Basta passare la stringa come argomento e la funzione restituirà il numero di caratteri all'interno della stringa.

```Elixir
String.length("Ciao, mondo!")
```

Output: `13`

Inoltre, puoi anche utilizzare il modulo `String` nella libreria standard di Elixir per accedere ad altre funzioni utili per le operazioni sulle stringhe.

```Elixir
String.slice("Buon giorno!", 0, 4)
```

Output: `"Buon"`

# Approfondimento

La funzione `String.length/1` utilizza un approccio efficace per trovare la lunghezza di una stringa. Invece di scorrere ogni carattere all'interno della stringa, utilizza la loro rappresentazione in binario per calcolare la lunghezza complessiva. Questo rende la funzione molto veloce e efficiente, soprattutto con stringhe di grandi dimensioni.

Inoltre, in Elixir è possibile utilizzare anche la macro `length/1` per trovare la lunghezza di una lista di elementi. Anche in questo caso, invece di scorrere ogni elemento, la macro utilizza la loro rappresentazione in binario per calcolare la lunghezza.

# Vedi anche

- [Elixir String module](https://hexdocs.pm/elixir/String.html)
- [Elixir List module](https://hexdocs.pm/elixir/List.html)
- [Blog post: Using String.length/1 for efficient string length calculations in Elixir](https://blog.lezzoo.dev/using-string-length-in-elixir)