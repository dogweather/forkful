---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perchè?

Generare numeri casuali significa creare una serie di numeri i cui pattern non possono essere prevedibili. I programmatori lo fanno per molte ragioni, come l'esecuzione di test randomizzati e l'implementazione di giochi.

## Come:

Ecco un semplice esempio su come generare numeri casuali in Gleam.

```gleam
import gleam/number.{random}
import gleam/io.{println}
import gleam/list
import gleam/result.{Ok}
import gleam/task

pub fn main() {
  task.async(Ok)
  |> task.map(_, fn(x) { number::random.between(1, 100) } )
  |> task.on_success(_, fn(x) { io::println(to_string(x)) } )
}
```

Ecco una possibile output di questo codice:

```gleam
// Output
42 
```

## Approfondimento:

Durante gli anni '40 e '50, i numeri casuali erano generati attraverso processi meccanici o elettronici, come lanciare dadi o selezionare carte casuali. Grazie ai progressi nel campo dell'elettronica e dell'informatica, oggi possiamo generare numeri casuali con programmazione.

In Gleam, il modulo di `gleam/number` si usa per generare numeri casuali. Tuttavia, ci sono anche altre alternative, come la utilizzazione di seminari o l'integrazione con librerie esterne.

Lo standard utilizzato da Gleam per i numeri casuali è basato su un algoritmo di Generatore di Numeri Pseudo-casuali che utilizza un seme per creare una serie di numeri apparentemente non collegati.

## Vedi Anche:

Per ulteriori informazioni sull'argomento, potete consultare questi link:

- Documentazione ufficiale di Gleam su "gleam/number": https://hexdocs.pm/gleam_stdlib/gleam/number.html 
- Articolo Wikipedia sull'argomento "Numeri casuali" : https://it.wikipedia.org/wiki/Numero_casuale
- Tutorial su come generare numeri casuali in Gleam: https://dev.to/thejoker131/how-to-generate-random-number-in-gleam-2ch8