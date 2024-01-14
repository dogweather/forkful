---
title:                "Elixir: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La modifica di testo è una pratica comune nella programmazione, che permette di automatizzare la sostituzione di determinati caratteri o stringhe all'interno di un file o di un codice. In Elixir, questa operazione è ancora più efficiente grazie alle potenti funzioni e pattern matching offerti dal linguaggio.

## Come fare

Per utilizzare la funzione di ricerca e sostituzione in Elixir, è necessario utilizzare il modulo `String`. Inoltre, è possibile specificare opzioni come la sensibilità al maiuscolo/minuscolo o la gestione dei caratteri di escape.

Un esempio semplice potrebbe essere questo:

```elixir
# Creazione di una stringa di prova
str = "Ciao a tutti!"

# Ricerca e sostituzione di una parola
new_str = String.replace(str, "Ciao", "Hello")

# Output: "Hello a tutti!"
IO.puts(new_str) 
```

Oltre alla funzione `replace`, esistono altre utile funzioni per la modifica di testo come `replace_at`, `replace_before` e `replace_after`. Si consiglia di consultare la documentazione ufficiale di Elixir per ulteriori dettagli e opzioni disponibili.

## Approfondimenti

In Elixir, la ricerca e sostituzione di testo può essere fatta non solo su stringhe, ma anche su altre strutture dati come liste e mappe. Inoltre, grazie all'utilizzo di pattern matching, è possibile personalizzare la logica di sostituzione in modo più flessibile.

Ad esempio, con la funzione `replace_match` è possibile specificare una lambda function che verrà applicata solo ai match trovati, come nel seguente esempio:

```elixir
# Creazione di una lista di numeri
list = [1, 2, 3]

# Ricerca e sostituzione solo dei numeri pari
new_list = Enum.map(list, fn
  0 -> "pari"
  x when rem(x, 2) == 0 -> "pari"
  x -> x
end)

# Output: ["pari", 2, 3]
IO.inspect(new_list)
```

Inoltre, l'utilizzo del modulo `Regex` permette di effettuare ricerche e sostituzioni basate su espressioni regolari, aggiungendo un ulteriore livello di precisione.

## Vedi anche

- [String module documentation](https://hexdocs.pm/elixir/String.html)
- [Official Elixir website](https://elixir-lang.org/)
- [Elixir School - String Functions](https://elixirschool.com/it/lessons/basics/string-functions/)