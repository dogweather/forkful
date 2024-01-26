---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:46.906856-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Unire stringhe, o concatenazione, significa metterle in fila una dopo l'altra. Programmare senza unire testi è come cucinare senza sale – si fa, ma spesso serve quel tocco in più.

## How to (Come fare)
In Gleam, puoi concatenare stringhe con il segno `++`. Semplice, guarda un esempio:

```gleam
pub fn main() {
  let saluto = "Ciao"
  let mondo = "mondo"
  let frase_completa = saluto ++ " " ++ mondo
  frase_completa
}
// Output: "Ciao mondo"
```

`frase_completa` è la combinazione di `saluto`, uno spazio, e `mondo`.

## Deep Dive (Approfondimento)
Concatenare stringhe è basico, ma fondamentale. I primi computer potevano solo sognare testi così agilmente manipolabili. In Gleam, ogni stringa è una sequenza di bytes UTF-8, quindi quando le unisci rispetti l'encoding.

Non è l'unico modo. Ci sono altri linguaggi con altri sistemi, come l'interpolazione o funzioni specifiche. In Gleam, concentriamoci su ciò che funziona meglio—`++`.

La concatenazione è lineare rispetto alla lunghezza delle stringhe—attenzione se lavori con testi lunghi!

## See Also (Vedi Anche)
- Per capire di più sull'encoding UTF-8: [UTF-8 su Wikipedia](https://it.wikipedia.org/wiki/UTF-8)
- Documentazione ufficiale di Gleam per le [stringhe](https://gleam.run/book/tour/strings.html)
- Alternativa per la gestione di stringhe più complesse: [template string in Rust](https://doc.rust-lang.org/std/fmt/) (Gleam si ispira a Rust!)
