---
title:                "Convertire una stringa in minuscolo"
html_title:           "Gleam: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Nella programmazione, convertire una stringa in minuscolo significa trasformare tutti i caratteri in lettere minuscole. Ciò è utile perché facilita il confronto e la manipolazione di stringhe, poiché non ci si deve preoccupare della differenza tra lettere maiuscole e minuscole.

## Come Fare:

Per convertire una stringa in minuscolo in Gleam, è possibile utilizzare la funzione `String.to_lowercase` come mostrato nell'esempio seguente:

```
Gleam...
import String

let my_string = "HELLO WORLD"
let lowercase_string = String.to_lowercase(my_string)

assert(lowercase_string == "hello world")
```

Il risultato di questa funzione è una nuova stringa con tutti i caratteri convertiti in minuscolo.

## Approfondimenti:

Nel corso degli anni, ci sono state molte implementazioni diverse per convertire una stringa in minuscolo. In alcuni linguaggi di programmazione, come Python, questo viene fatto utilizzando un metodo specifico per le stringhe, mentre in altri, come Java, è necessario utilizzare una classe apposita. In Gleam, la funzione `String.to_lowercase` utilizza un approccio semplice e intuitivo per eseguire questa conversione.

## Vedi anche:

Per ulteriori informazioni sui metodi di manipolazione delle stringhe in Gleam, è possibile consultare la documentazione offerta dal sito ufficiale [Gleam](https://gleam.run/). Inoltre, è possibile esplorare gli altri metodi di manipolazione delle stringhe che Gleam offre, come ad esempio `String.to_uppercase` per convertire una stringa in maiuscolo.