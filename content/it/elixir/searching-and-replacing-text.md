---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

La ricerca e la sostituzione del testo sono processi utilizzati per manipolare le stringhe nel codice. Gli sviluppatori li utilizzano per sostituire parti specifiche del testo, per automatizzare ed ottimizzare il loro lavoro.

## Come Fare

Ecco come eseguire un'operazione di ricerca e sostituzione in Elixir (versione attuale):

```elixir
defmodule TestString do
  def replace(text, target, replacement) do
    String.replace(text, target, replacement)
  end
end

IO.puts TestString.replace("Ciao, mondo!", "mondo", "Elixir")
```

Quando esegui l'esempio di codice fornito sopra, il risultato sarà:

```elixir
"Ciao, Elixir!"
```

## Analisi Dettagliata

(1) La ricerca e la sostituzione del testo risalgono agli albori dell'informatica, quando gli sviluppatori manipolavano testo con espressioni regolari e codice assembly.

(2) In Elixir, si possono utilizzare funzioni di stringa built-in come `String.replace/3` per la ricerca e la sostituzione del testo, ma esistono anche alternative come l'uso di espressioni regolari con `Regex.replace/4`.

(3) Il metodo `String.replace/3` in Elixir è implementato come una ricerca lineare attraverso la stringa data, confrontando ogni carattere con il target. Quando trova una corrispondenza, sostituisce il target con il testo di sostituzione.

## Vedi Anche

Per ulteriori informazioni su String.replace/3, consulta la [documentazione ufficiale di Elixir](https://hexdocs.pm/elixir/String.html#replace/3).

Per conoscere le espressioni regolari in Elixir, dai un'occhiata a [questo articolo](https://elixirschool.com/en/lessons/advanced/regex/). 

Per capire meglio come le stringhe funzionano in Elixir, visita [questo link](https://elixirschool.com/en/lessons/basics/strings/).