---
title:                "Estrarre sottostringhe"
html_title:           "Gleam: Estrarre sottostringhe"
simple_title:         "Estrarre sottostringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Estrarre sottostringhe è un'operazione molto comune nella programmazione, in cui si selezionano parti specifiche di una stringa più grande per essere utilizzate nel codice. I programmatori fanno questo per rendere più semplice la manipolazione dei dati e per ottenere solo le informazioni rilevanti di cui hanno bisogno.

## Come fare:

```
Gleam.string.slice("Hello World", 0, 5)
```
Questo codice restituirà "Hello", che è una sottostringa delle prime cinque lettere della stringa originale.

```
Gleam.string.drop("Hello World", 6)
```
Questo codice restituirà "World", data una stringa di partenza di "Hello World" e una lunghezza di 6, la prima parte ("Hello ") verrà eliminata e verrà restituita solo la seconda parte ("World").

## Approfondimento:

Le estrazioni di sottostringhe non sono una novità nella programmazione e sono disponibili in vari linguaggi di programmazione come Java e Python. Tuttavia, Gleam ha la sua implementazione unica e semplice che rende l'estrazione di sottostringhe ancora più facile per i programmatori.

Alcune alternative all'estrazione di sottostringhe includono l'utilizzo di metodi di manipolazione delle stringhe come la sostituzione o la rimozione di caratteri, ma l'utilizzo delle funzioni di estrazione di sottostringhe è più chiaro e intuitivo.

In Gleam, l'implementazione delle funzioni di estrazione di sottostringhe avviene utilizzando gli indici dei caratteri della stringa, che possono essere specificati dall'utente.

## Vedi anche:

- Documentazione Gleam per il modulo `String`: https://gleam.run/documentation/stdlib/string/
- Articolo su esempi di estrazione di sottostringhe in Java: https://www.programiz.com/java-programming/examples/string-substring