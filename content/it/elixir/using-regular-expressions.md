---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Le espressioni regolari, conosciute come `Regex`, sono una potente sintassi per cercare, abbinare e sostituire una parte di una stringa. Gli sviluppatori le utilizzano per manipolare dati testuali o convalidare gli input secondo specifici formati.

## Come fare:

Ecco alcuni esempi di come utilizzare le espressioni regolari in Elixir.

```elixir
# Definire una regex
re = ~r/ciao/

# Convalidare una stringa
String.match?("Ciao Mondo", re)   # true

# Sostituire una stringa
String.replace("Ciao Mondo", re, "Salve") # "Salve Mondo"
```

## Approfondimento

Historicalmente, le `Regex` risalgono al 1950 e sono diventate un'importante strumento di programmazione grazie alla loro inclusione nei linguaggi più popolari, tra cui Perl, JavaScript e, naturalmente, Elixir.

Sebbene le espressioni regolari siano molto utili, esistono alternative nel caso in cui la sintassi risulti troppo complessa o la performance sia una preoccupazione. Ad esempio, potresti considerare l'uso di metodi string-specifici (`index_of`, `split`, ecc.) o librerie di analisi di stringhe dedicate.

In Elixir, le espressioni regolari sono implementate attraverso la libreria standard `:re` di Erlang, offrendo piena compatibilità con la sintassi delle espressioni regolari di Perl (conosciuta come PCRE).

## Guarda anche

Per approfondire le modalità di utilizzo delle espressioni regolari in Elixir, consulta queste risorse:

1. Documentazione ufficiale di Elixir: [Regex](https://hexdocs.pm/elixir/Regex.html)
2. Erlang/OTP: [modulo `:re`](http://erlang.org/doc/man/re.html)