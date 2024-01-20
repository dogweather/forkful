---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché? 

L'interpolazione delle stringhe in programmazione è un processo dove combiniamo variabili e stringhe. I programmatori lo fanno per creare dinamicamente i dati testuali, rendendo l'output più facile da leggere e da manipolare.

## Come si fa:

Ecco un esempio di come eseguire l'interpolazione delle stringhe in Gleam:

```Gleam
fn main() {
  let nome = "Mario"
  let eta = 30
  Io.print("Ciao, {nome}! Hai {eta} anni.", [nome, eta])
}
```

L'output sarà:

```ui.dialog
Ciao, Mario! Hai 30 anni.
```

## Approfondimento

(1) Storicamente, l'interpolazione delle stringhe risale ai primi giorni della programmazione, con la necessità di combinare insieme dati variabili e testi fissi.

(2) Ci sono molte alternative all'interpolazione delle stringhe, ad esempio la concatenazione delle stringhe. Tuttavia, l'interpolazione delle stringhe è più leggibile e meno incline a errori.

(3) In Gleam, l'interpolazione delle stringhe avviene tramite la funzione `Io.print()`. I valori all'interno delle parentesi graffe { } vengono sostituiti dai valori specificati nell'elenco di argumenti alla fine della funzione.

## Vedere Anche
