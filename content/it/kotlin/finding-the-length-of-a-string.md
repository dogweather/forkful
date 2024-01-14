---
title:                "Kotlin: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
La lunghezza di una stringa è un concetto fondamentale nella programmazione e comprendere come ottenerla è importante per manipolare efficacemente i dati in un programma Kotlin.

## Come fare
Per trovare la lunghezza di una stringa in Kotlin, possiamo utilizzare il metodo `length()` come mostrato nell'esempio seguente:
```
Kotlin val stringa = "Ciao Mondo!"
println("La lunghezza della stringa è ${stringa.length()}")
```
L'output del programma sarà:
```
La lunghezza della stringa è 11
```
Come si può osservare, il metodo `length()` restituisce un intero che rappresenta la lunghezza della stringa. Possiamo utilizzare questo valore per eseguire operazioni e controlli all'interno del nostro programma.

## Approfondimento
In Kotlin, le stringhe sono immutabili, il che significa che una volta create, non possono essere modificate. Quando utilizziamo il metodo `length()`, non stiamo modificando la stringa originale ma semplicemente ottenendo una sua caratteristica. Inoltre, il metodo `length()` è disponibile per qualsiasi tipo di stringa e può essere utilizzato anche senza assegnare la stringa a una variabile.

## Vedi anche
- [Documentazione di Kotlin su `length()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-char-sequence/length.html)
- [Tutorial su Kotlin per principianti](https://www.codingame.com/playgrounds/39236/le-basi-di-kotlin/introduzione)