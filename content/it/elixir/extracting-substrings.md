---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

In Elixir, l'estrazione di sottostringhe è l'operazione di presa di una stringa e la divisione in parti più piccole, propagando da un indice di inizio a uno di fine all'interno della stringa originale. Gli sviluppatori lo fanno per manipolare, analizzare e ristrutturare i dati di testo.

## Come si fa:

In Elixir, è possibile utilizzare la funzione `String.slice/3` per estrarre sottostringhe. Questa funzione prende la stringa originale e due indici come argomenti.

```Elixir
iex> stringa_orig = "Salve, come va?"
iex> String.slice(stringa_orig, 0, 5)
"Salve"
```
L'output di questo codice sarà:
"Salve"

## Approfondimento

La funzione `String.slice/3` in Elixir si basa sulla semantica Erlang per le stringhe, visto che Elixir è costruito su Erlang. Altre funzioni simili per manipolare le stringhe includono `String.substring/2` e `String.at/2`.

In termini di implementazione, le stringhe in Elixir non sono niente di più che liste di code point Unicode. Il conteggio degli indici inizia da zero, e vengono contati i code point, non i byte.

C'è da notare che Elixir offre metodi più avanzati e potenti per lavorare con le stringhe, come l'utilizzo delle espressioni regolari attraverso il modulo `Regex`.

## Vedi anche

Per saperne di più sulle stringhe e loro manipolazioni in Elixir:

https://hexdocs.pm/elixir/String.html

https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html

https://elixir-lang.org/crash-course.html#strings