---
title:                "Trova la lunghezza di una stringa"
html_title:           "Elixir: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Che cosa & Perché?

La ricerca della lunghezza di una stringa è un'operazione comune nelle applicazioni di programmazione. La lunghezza di una stringa indica il numero di caratteri presenti all'interno di essa e può essere utile per diverse funzionalità come la valutazione della validità dei dati inseriti dall'utente o la creazione di algoritmi di ricerca.

Come fare:

Elixir rende facile il calcolo della lunghezza di una stringa grazie al metodo `String.length`. Basta passare la stringa come argomento e il metodo restituirà il numero di caratteri.

```Elixir
iex> String.length("Ciao mondo!")
11
```

È anche possibile utilizzare la funzione `String.length` su una lista di caratteri, che restituirà la lunghezza della lista stessa.

```Elixir
iex> String.length(['h', 'e', 'l', 'l', 'o'])
5
```

Deep Dive:

Il calcolo della lunghezza di una stringa è un'operazione molto comune e fondamentale nella programmazione. Prima di Elixir, questo era spesso fatto manualmente attraverso la scansione della stringa e il conteggio dei caratteri. Tuttavia, il metodo `String.length` di Elixir rende questa operazione molto più efficiente.

In alternativa, è possibile utilizzare il metodo `length/1` della libreria standard di Erlang. Ciò consente di calcolare la lunghezza di qualsiasi tipo di lista, non solo le stringhe.

```Elixir
iex> length([1,2,3])
3
```

Si noti che la lunghezza di una stringa può variare a seconda della codifica dei caratteri utilizzata, ad esempio UTF-8 o ASCII. Elixir gestisce questa differenza e restituirà il valore corretto.

Vedi anche:

- Documentazione ufficiale Elixir: https://hexdocs.pm/elixir/String.html#length/1
- Documentazione ufficiale Erlang: https://erlang.org/doc/man/stdlib.html#length-1