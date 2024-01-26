---
title:                "Trovare la lunghezza di una stringa"
date:                  2024-01-20T17:47:13.635411-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Scoprire la lunghezza di una stringa significa semplicemente contare quanti caratteri contiene. I programmatori lo fanno per validare l'input, gestire il testo nelle UI, o per eseguire operazioni di elaborazione delle stringhe come il taglio o il riempimento.

## Come si fa:
```Gleam
import gleam/io

pub fn main() {
  let saluto = "Ciao!"
  let lunghezza = String.length(saluto)
  io.println(lunghezza) // 5
}
```

## Approfondimento
Trovar la lunghezza di una stringa è un'operazione di base in programmazione, presente fin dagli albori dei linguaggi informatici. Gleam, essendo un linguaggio moderno di tipi funzionali, offre una funzione `length` integrata nella libreria standard. In contrasto con i linguaggi più vecchi dove si potrebbe trovare il terminatore null in C, in Gleam la lunghezza di una stringa è direttamente disponibile. Le alternative includono il ciclo su ogni carattere, ma ciò è piú dispendioso e raramente necessario in Gleam grazie alla sua API robusta.

## Vedi anche
- Documentazione ufficiale di Gleam su stringhe: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Introduzione a Gleam per chi parla italiano: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
