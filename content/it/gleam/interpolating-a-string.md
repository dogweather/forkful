---
title:                "Interpolazione di una stringa"
html_title:           "Gleam: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Interpolazione di stringhe è un modo per inserire valori variabili all'interno di una stringa. I programmatori usano questa tecnica per semplificare il processo di creazione di stringhe dinamiche, rendendo il codice più leggibile e mantenibile. 

## Come fare:
Utilizzare l'operatore `~` per inserire una variabile all'interno di una stringa. Ad esempio:
```Gleam
let nome = "Maria"
let saluto = "Ciao, ~!"
```
L'uscita sarà "Ciao, Maria!".
Si possono anche concatenare più variabili utilizzando `~`.
```Gleam
let nome = "Maria"
let cognome = "Rossi"
let saluto = "Ciao, ~ ~!"
```
L'uscita sarà "Ciao, Maria Rossi!".

## Approfondimento:
Interpolazione di stringhe è una tecnica molto comune utilizzata in numerosi linguaggi di programmazione come Javascript, Python e Ruby. In alcuni linguaggi, invece di utilizzare `~`, si utilizza il carattere `\` per indicare una variabile all'interno di una stringa.
Alcune alternative alla interpolazione di stringhe includono la formattazione di stringhe e la concatenazione di stringhe manualmente, ma queste possono essere meno efficienti e più soggette a errori.
Nel codice sorgente di Gleam, l'interpolazione di stringhe è implementata come una funzione che accetta una stringa di formato e un numero variabile di argomenti da inserire al suo interno.

## Vedi anche:
- [Documentazione di Gleam su interpolazione di stringhe](https://gleam.run/book/tour/interpolation.html)
- [Pagine di riferimento sulle stringhe in Gleam](https://gleam.run/book/std/string.html)