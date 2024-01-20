---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cosa e Perché?
L'estrazione di sottostringhe è l'atto di ottenere una stringa più piccola da una stringa più grande. I programmatori lo fanno per manipolare e lavorare con parti specifiche di stringhe, che è fondamentale in molte operazioni di programmazione.

## Come Fare:
Ecco un esempio di come estrarre sottostringhe in Gleam:

```Gleam
import gleam/string

fn main() {
let s = "Ciao, Mondo!"
let parte = string.slice(s, 0, 4) 
println(parte) // Stampa "Ciao"
}
```
Nell'esempio, `string.slice(s, 0, 4)` estre la sottostringa di `s` da indizio 0 a 4.

## Un Tuffo Più Profondo:
Historicamente, l'estrazione di sottostringhe è una funzione fondamentale negli linguaggi di programmazione. In Gleam, usiamo `string.slice` per estrarre sottostringhe, ma ci sono alternative come `string.drop` e `string.take` che eliminano o prendono un certo numero di caratteri dalla stringa. Ricorda, gli indici in Gleam iniziano da 0, non da 1.

## Vedi Anche:
2. [An introduction to Gleam](https://gleam.run/book/)

Non esitate a sperimentare su [Gleam's Online Playground](https://play.gleam.run/) per capire meglio come si estraggono le sottostringhe!