---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

La concatenazione delle stringhe è l'atto di unire due o più stringhe in una unica. I programmatori lo fanno per semplificare e ottimizzare il codice, specialmente quando si lavora con grandi quantità di dati testuali.

## Come Fare:

Ecco un esempio che mostra come concatenare stringhe in Elixir:

```Elixir
str1 = "Ciao, "
str2 = "mondo!"
str3 = str1 <> str2
IO.puts str3
```

L'output sarà:

```Elixir
Ciao, mondo!
```

In Elixir, utilizziamo l'operatore `<>` per concatenare le stringhe. Puoi concatenare quanti più stringhe desideri in una sola volta.

## Approfondimenti:

Elixir, presentato per la prima volta nel 2011, ha adottato la concatenazione delle stringhe come un modo elegante e semplice per manipolare le stringhe. Un'alternativa sarebbe l'uso di funzioni come `String.concat/2`, ma l'utilizzo di `<>` risulta più pulito ed efficace. In termini di dettagli implementativi, Elixir lavora con stringhe come "binari" per migliorate le prestazioni e l'efficienza.

## Leggi Anche:

1. Documentazione ufficiale di Elixir sulla concatenazione delle stringhe: [https://hexdocs.pm/elixir/String.html#concat/2](https://hexdocs.pm/elixir/String.html#concat/2)
3. Discussione su Stackoverflow sulla concatenazione delle stringhe in Elixir: [https://stackoverflow.com/questions/27398325/how-to-concatenate-strings-in-elixir](https://stackoverflow.com/questions/27398325/how-to-concatenate-strings-in-elixir)