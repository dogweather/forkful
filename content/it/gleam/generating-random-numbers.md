---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:04.421853-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa produrre valori non previsti da un programma. Programmatori lo fanno per giochi, simulazioni, test, sicurezza e ogni volta che serve elemento sorpresa.

## How to:
Ecco un esempio semplice:

```gleam
import gleam/io
import gleam/random

fn main() {
  let seed = random.default_seed()
  let (numero_casuale, _nuovo_seme) = random.int(seed, 1, 100)
  io.println(numero_casuale)
}
```
Output potrebbe essere un numero qualunque tra 1 e 100, tipo `42`.

## Deep Dive
Prima del computer, generare numeri "casuali" era questione di lanciare dadi o usare strumenti analogici. Ora, usiamo algoritmi, ma attenzione: in realtà sono sequenze determinate che sembrano casuali, da qui "pseudo-random". Gleam si appoggia a BEAM (Erlang virtual machine), il quale ha un buon supporto per generazione di numeri pseudo-casuali. Rispetto ad altri linguaggi, Gleam è più giovane, ma i suoi strumenti si basano su decenni di ricerca e sviluppo dal mondo Erlang. Altre opzioni includono generare numeri con sistemi hardware o funzioni crittografiche per maggiore casualità.

## See Also
- Erlang's documentation on random numbers for historical context: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)
- An explanation on pseudo-random generation in programming: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
