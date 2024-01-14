---
title:    "Elixir: Convertire una stringa in minuscolo"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
La conversione di una stringa in minuscolo può essere utile in diversi casi, come ad esempio la normalizzazione dei dati prima di una elaborazione o la manipolazione di input utente.

## Come fare
```Elixir
# Utilizziamo la funzione String.downcase per convertire una stringa in minuscolo
stringa = "CIAO A TUTTI"
risultato = String.downcase(stringa)

IO.puts("Output: #{risultato}") # Output: ciao a tutti
```

## Approfondimento
La funzione String.downcase è una delle tante funzioni disponibili nel modulo String di Elixir. Essa utilizza il concetto di Unicode per gestire correttamente le lettere accentate o caratteri speciali. Inoltre, è possibile passare un parametro opzionale per specificare la lingua da utilizzare per la conversione.

## Vedi anche
- [Documentazione ufficiale della funzione String.downcase](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Tutorial su Unicode in Elixir](https://elixir-lang.org/getting-started/unicode-charlists-and-codepoints.html)
- [Tutorial sulla gestione delle stringhe in Elixir](https://elixir-lang.org/getting-started/strings.html)