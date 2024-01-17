---
title:                "Maiuscolare una stringa"
html_title:           "Gleam: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Gleam: Maiuscola Stringhe in Modo Semplice

## Cosa & Perché?

Capitare una stringa significa convertirla in maiuscolo. I programmatori fanno questo per una varietà di ragioni, come uniformare l'aspetto delle stringhe nei loro codici o per conformarsi a particolari convenzioni di sintassi.

## Come Fare:

In Gleam, possiamo utilizzare la funzione `String.to_uppercase` per capitalizzare una stringa, come mostrato nell'esempio seguente:

```Gleam
let stringa = "ciao";
let stringa_maiuscola = String.to_uppercase(stringa);
```

Il risultato sarà "CIAO" nella variabile `stringa_maiuscola`.

Possiamo anche capitalizzare una stringa in situ, utilizzando l'operatore di assegnazione `=` insieme alla funzione `String.to_uppercase`:

```Gleam
let stringa = "ciao";
stringa = String.to_uppercase(stringa);
```

In entrambi gli esempi, abbiamo assegnato il risultato della funzione `String.to_uppercase` a una nuova variabile o alla variabile originale. Questo perché le stringhe sono immutabili in Gleam, il che significa che non possono essere modificate una volta create.

## Analisi Approfondita:

In passato, la conversione in maiuscolo di una stringa richiedeva l'utilizzo di funzioni di libreria complesse o di codice personalizzato. Con Gleam, questo è stato semplificato notevolmente grazie alla funzione `String.to_uppercase`.

In alternativa, gli sviluppatori possono sfruttare le funzioni di convenzione di sintassi disponibili in diversi editor di testo e IDE per aiutare nella capitalizzazione delle stringhe.

Per quanto riguarda l'implementazione, la funzione `String.to_uppercase` utilizza l'algoritmo Unicode per la normalizzazione dei caratteri e per identificare i caratteri che devono essere convertiti in maiuscolo.

## Vedi Anche:

- [Documentazione Gleam per `String.to_uppercase`](https://gleam.run/core/string#toupper/1)
- [Unicode.org](https://unicode.org/versions/Unicode13.0.0/) per informazioni sull'algoritmo Unicode.