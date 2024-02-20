---
date: 2024-01-20 17:52:29.351062-07:00
description: "Stampare l'output di debug significa mostrare temporaneamente i valori\
  \ in fase di esecuzione per capire che diavolo sta facendo il codice. Si fa per\u2026"
lastmod: 2024-02-19 22:05:02.196017
model: gpt-4-1106-preview
summary: "Stampare l'output di debug significa mostrare temporaneamente i valori in\
  \ fase di esecuzione per capire che diavolo sta facendo il codice. Si fa per\u2026"
title: Stampa dell'output di debug
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Stampare l'output di debug significa mostrare temporaneamente i valori in fase di esecuzione per capire che diavolo sta facendo il codice. Si fa per identificare e risolvere i bug più velocemente.

## How to: (Come fare:)
```elixir
# Stampa semplice.
IO.puts("Qualcosa sta succedendo qui")
# Output: Qualcosa sta succedendo qui

# Debug con dati.
data = %{name: "Guido", age: 42}
IO.inspect(data, label: "Dati utente")
# Output: Dati utente: %{age: 42, name: "Guido"}
```

## Deep Dive (Approfondimento)
Elixir, nato nel 2011, consente di eseguire debug facilmente grazie a funzioni come `IO.puts` e `IO.inspect`. `IO.inspect` è particolarmente potente perché puoi etichettare l'output e limitare la visualizzazione dei dati con opzioni come `:limit`. A differenza di altri linguaggi, come C che usa `printf`, o Python con `print`, in Elixir `IO.inspect` ritorna anche il valore originale, quindi puoi inserirlo ovunque nel tuo codice senza interrompere il flusso dei dati.

Alternative? Scordatele. Strumenti come `:observer` sono potenti per un controllo approfondito, ma per un veloce check, `IO.puts` e `IO.inspect` sono imbattibili. In termini di implementazione, queste funzioni gestiscono l'output in modo asincrono, quindi potrebbero non apparire in ordine se le esegui in processi Elixir paralleli.

## See Also (Vedi Anche)
- [Elixir School - Basic](https://elixirschool.com/en/lessons/basics/basics/)
- [Elixir Documentation - IO Module](https://hexdocs.pm/elixir/IO.html)
- [Debugging techniques in Elixir](https://elixir-lang.org/getting-started/debugging.html)
