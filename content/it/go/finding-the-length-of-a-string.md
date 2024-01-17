---
title:                "Trova la lunghezza di una stringa"
html_title:           "Go: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione, che consiste nel determinare il numero di caratteri presenti in una stringa. I programmatori spesso hanno bisogno di conoscere la lunghezza delle stringhe per svolgere operazioni come la manipolazione dei dati o la validazione degli input.

## Come fare: 
```Go
lunghezza := len("Ciao mondo!")
fmt.Println(lunghezza)
```

Questo codice restituirà l'output "11", poiché "Ciao mondo!" è una stringa di 11 caratteri. È possibile utilizzare la funzione ```len()``` su qualsiasi stringa in Go per ottenere la sua lunghezza.

## Approfondimento: 
Inizialmente, la maggior parte dei linguaggi di programmazione aveva una funzione separata per calcolare la lunghezza delle stringhe. Tuttavia, con l'avvento dei linguaggi orientati agli oggetti, molti linguaggi hanno incorporato un metodo ```length()``` che può essere chiamato su un oggetto stringa per ottenere la sua lunghezza. Tuttavia, in Go, la funzione ```len()``` rimane l'unica opzione.

Ci sono anche alcune alternative alla funzione ```len()``` in Go, come ad esempio utilizzare il pacchetto ```unicode/utf8``` per gestire stringhe Unicode o la funzione ```bytes.Count()``` per contare il numero di byte in una stringa. Tuttavia, per la maggior parte dei casi, la semplicità e la velocità della funzione ```len()``` la rendono la scelta migliore per la maggior parte dei programmatori.

Per quanto riguarda l'implementazione della funzione ```len()``` in Go, essa è direttamente collegata all'allocazione di memoria delle stringhe e non computa ogni volta la lunghezza della stringa da zero. Ciò significa che il calcolo della lunghezza della stringa è molto efficiente e non ha un impatto negativo sulle prestazioni del programma.

## Vedi anche: 
- Documentazione ufficiale di Go sulla funzione ```len()```: https://golang.org/pkg/builtin/#len
- Altro dettagli su come le stringhe sono gestite in Go: https://golang.org/doc/articles/strings.html