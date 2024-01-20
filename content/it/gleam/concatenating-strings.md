---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La concatenazione di stringhe è il processo di unire due o più stringhe insieme. I programmatori lo fanno per costruire stringhe più complesse a partire da parti più piccole e gestibili.

## Come fare:

Ecco un esempio della concatenazione di stringhe in Gleam:

```Gleam
let welcome = "Ciao, "
let name = "Mario"
let greeting = welcome ++ name
print(greeting)
```

Questo programma stampa `Ciao, Mario`.

## Approfondimento

Storicamente, la concatenazione di stringhe è sempre stata un'operazione fondamentale nel campo della programmazione. Tuttavia, la sua implementazione può variare notevolmente tra diversi linguaggi.

In Gleam, la concatenazione di stringhe si realizza utilizzando l'operatore `++`. Questa è una scelta di design che può risultare familiare agli sviluppatori Erlang ed Elixir, anch'essi basati sulla piattaforma BEAM.

Un'alternativa alla concatenazione di stringhe è l'uso della formattazione delle stringhe, che può offrire una maggiore flessibilità in alcune situazioni. Ecco come potrebbe sembrare con Gleam:

```Gleam
let name = "Mario"
let greeting = string.concat(["Ciao, ", name])
print(greeting)
```

## Guarda Anche

Per ulteriori informazioni sulla programmazione in Gleam, controlla i seguenti link:

- [Esercizi pratici su Gleam](https://exercism.io/tracks/gleam)
- [FAQ di Gleam](https://github.com/gleam-lang/gleam/wiki/FAQs)