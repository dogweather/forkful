---
title:                "Mettere in maiuscolo una stringa"
html_title:           "Elixir: Mettere in maiuscolo una stringa"
simple_title:         "Mettere in maiuscolo una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Introduzione alla Capitalizzazione delle Stringhe in Elixir

## Cosa & Perché?
La capitalizzazione di una stringa implica la trasformazione del primo carattere di un testo in una lettera maiuscola. I programmatori lo fanno per rendere il testo facilmente leggibile, o per rispettare certe convenzioni di formattazione.

## Come fare:
In Elixir, capita spesso di dover capitalizzare una stringa. Per farlo, si può utilizzare la funzione `String.capitalize/2`. Ecco un esempio di utilizzo:

```Elixir
IO.puts String.capitalize("ciao mondo") 
```

Output:

```Elixir
"Ciao mondo"
```

## Approfondimento
Elixir non ha sempre avuto una funzione di capitalizzazione delle stringhe. Questa è stata aggiunta nella versione 1.5, rilasciata a luglio 2017. 

Come alternativa, potresti voler provare ad utilizzare la libreria Erlang :unicode. Geradrebbe un carattere maiuscolo in codifica Unicode.

Dettagli di implementazione: la funzione `String.capitalize/2` in Elixir usa algoritmi unicode per la capitalizzazione. Questo significa che può gestire correttamente anche stringhe non ascii, come "ciao mondo" in italiano.

## Vedi Anche:
Se vuoi approfondire, qui ci sono alcune risorse correlate:

1. La documentazione ufficiale di Elixir sulla funzione String.capitalize: (https://hexdocs.pm/elixir/String.html#capitalize/2)

2. Un'interessante discussione su StackOverflow sulla capitalizzazione delle stringhe in Elixir: (https://stackoverflow.com/questions/46515888/how-can-i-capitalize-a-string-in-elixir)

3. Un tutorial su come formattare stringhe in Elixir: (https://elixirschool.com/en/lessons/basics/strings/)