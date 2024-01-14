---
title:                "Elixir: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune nel mondo della programmazione. Sapere quanto è lunga una stringa può essere utile per molteplici scopi, come la validazione di input, il calcolo delle dimensioni di un array o anche solo per scopi di visualizzazione.

## Come Fare

Per trovare la lunghezza di una stringa in Elixir, possiamo utilizzare la funzione `String.length/1`. Prende come argomento la stringa di cui vogliamo trovare la lunghezza e restituisce un intero rappresentante il numero di caratteri.

```Elixir 
iex> String.length("Ciao") 
4 
```

Se vogliamo includere anche gli spazi bianchi nel conteggio, possiamo utilizzare invece la funzione `String.length/2` e passare anche l'opzione `:utf8` come secondo argomento.

```Elixir 
iex> String.length("Ciao, come stai?", :utf8) 
16
```

## Approfondimento

Mentre la funzione `String.length/1` sembra semplice e diretta, ci sono alcuni dettagli interessanti da considerare. Innanzitutto, Elixir rappresenta le stringhe utilizzando il formato Unicode UTF-8, che significa che ogni carattere può essere rappresentato da più byte.

Inoltre, la funzione `String.length/2` in realtà non conta gli spazi bianchi, ma i codepoint, ovvero i singoli caratteri. Quindi, se ad esempio la nostra stringa contiene un carattere UTF-8 che occupa più di un byte, verrà conteggiato come un solo codepoint.

Infine, è importante sottolineare che la lunghezza restituita da `String.length/1` e `String.length/2` è sempre un intero, anche se la stringa contiene solo numeri. Se vogliamo convertirla in un float, possiamo utilizzare la funzione `String.to_float/1`.

## Vedi Anche

- [La documentazione ufficiale su `String.length/1`](https://hexdocs.pm/elixir/String.html#length/1)
- [Un articolo su UTF-8 e codepoint in Elixir](https://medium.com/@jlouis666/elixir-strings-unicode-codepoints-and-bytes-6abfaf08a052)
- [Un tutorial su come convertire una stringa in un float in Elixir](https://elixirschool.com/lessons/basics/binary-and-strings/#converting-a-string-to-a-float)