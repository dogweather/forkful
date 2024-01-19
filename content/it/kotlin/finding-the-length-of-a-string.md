---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Determinare la lunghezza di una stringa nel linguaggio di programmazione Kotlin significa contare il numero di caratteri in una determinata sequenza di testo. I programmatori lo fanno per manipolare o valutare dati testuali - come il conteggio dei caratteri in una password o l'assegnazione di un valore basato sulla lunghezza del testo.

## Come fare:

Ecco un esempio di come trovare la lunghezza di una stringa in Kotlin:

```Kotlin
fun main() {
    val nome = "Giuseppe"
    val lunghezzaNome = nome.length
    println("La lunghezza del nome è $lunghezzaNome")
}
```

In output otteniamo:

```Kotlin
La lunghezza del nome è 8
```

## Analisi Dettagliata

Kotlin, evoluzione di Java, ha semplificato la determinazione della lunghezza di una stringa rispetto ai linguaggi predecessori. In alternativa, con la funzione `count()` si può addirittura calcolare il numero di elementi in una stringa che soddisfano una certa condizione. Ecco un esempio:

```Kotlin
fun main() {
    val nome = "Giuseppe"
    val numeroE = nome.count { it == 'e' }
    println("Il numero di 'e' nel nome è $numeroE")
}
```

In output otteniamo:

```Kotlin
Il numero di 'e' nel nome è 2
```

Per implementazione, Kotlin usa internamente la classe `java.lang.String` di Java per le operazioni sulle stringhe, inclusa la misura della loro lunghezza. È interessante notare che Kotlin considera anche i caratteri Unicode come un unico elemento per la lunghezza della stringa.

## Guarda Anche:

1. [Documentazione di Kotlin sulle Stringhe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
2. [Tutorial sulla gestione delle stringhe in Kotlin](https://www.programiz.com/kotlin-programming/string)
3. [Documentazione Java API su Unicode](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)