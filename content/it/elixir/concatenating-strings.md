---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:37.160329-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Concatenare le stringhe significa unire due o più sequenze di caratteri per formarne una sola. I programmatori lo fanno per creare messaggi dinamici, manipolare testi e combinare dati.

## Come fare:
```elixir
# Concatenazione con l'operatore <>
saluto = "Ciao, "
nome = "Marco!"
messaggio = saluto <> nome
IO.puts messaggio  # Output: Ciao, Marco!

# Concatenazione con la funzione IO.iodata_to_binary
lista_di_stringhe = ["Buon", " giorno", " a", " tutti!"]
stringa_unica = IO.iodata_to_binary(lista_di_stringhe)
IO.puts stringa_unica # Output: Buon giorno a tutti!
```

## Approfondimento
La concatenazione di stringhe in Elixir è efficiente grazie alle liste di caratteri (charlists) e il modello binario di Erlang su cui si basa Elixir. Mentre in alcuni linguaggi concatenare stringhe può essere costoso, Elixir usa una struttura di dati immutabile che rende l'operazione meno onerosa. Esistono alternative come l'interpolazione (`"#{var1} #{var2}"`), ma la concatenazione diretta tramite `<>` è chiara e veloce. In passato, la gestione delle stringhe differiva notevolmente tra i linguaggi di programmazione ma, con le moderne ottimizzazioni, ora è più una questione di sintassi e preferenza personale.

## Vedi anche:
- Documentazione ufficiale Elixir su stringhe e operazioni di concatenazione: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Una discussione su come Elixir gestisce le stringhe internamente: [https://elixirforum.com/](https://elixirforum.com/)
- Un confronto delle prestazioni di concatenazione delle stringhe tra linguaggi diversi: [https://benchmarksgame-team.pages.debian.net/benchmarksgame/](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)