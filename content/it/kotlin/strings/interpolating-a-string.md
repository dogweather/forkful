---
date: 2024-01-20 17:51:25.939076-07:00
description: "How to: Kotlin rende l'interpolazione di stringhe piacevole e semplice.\
  \ Usa il simbolo del dollaro (`$`) seguito dal nome di una variabile, o graffe\u2026"
lastmod: '2024-03-13T22:44:43.378285-06:00'
model: gpt-4-1106-preview
summary: Kotlin rende l'interpolazione di stringhe piacevole e semplice.
title: Interpolazione di una stringa
weight: 8
---

## How to:
Kotlin rende l'interpolazione di stringhe piacevole e semplice. Usa il simbolo del dollaro (`$`) seguito dal nome di una variabile, o graffe (`{}`) per espressioni, e sei a cavallo. Ecco come:

```Kotlin
fun main() {
    val linguaggio = "Kotlin"
    val versione = 1.4

    println("Impariamo $linguaggio in questo momento!")
    println("La versione corrente è ${versione}")
}
```

Output:
```
Impariamo Kotlin in questo momento!
La versione corrente è 1.4
```

## Deep Dive
Interpolare significa letteralmente "inserire (qualcosa di diverso) tra altre cose". In programmazione, è come se stessimo inserendo dei 'pezzetti' di dati all'interno di un fluente testo. Un po' di storia: questa pratica non è nuova e la trovi in molti linguaggi, come Perl o PHP, dove esisteva già agli albori degli anni '90.

Ci sono alternative? Certamente. Puoi concatenare elementi con il classico `+`, ma nei casi complessi diventa un lavoraccio e puoi dire addio alla leggibilità. Kotlin ha spinto molto sull'interpolazione perché permette di mantenere il codice pulito e performante.

Kotlin, come altri linguaggi moderni, compila l'interpolazione di stringhe in modo efficiente, così non devi preoccuparti di problemi di prestazioni. La JVM, che sta sotto al cofano, gestisce questa ottimizzazione.

## See Also
- Documentazione ufficiale Kotlin per l'interpolazione di stringhe: [Kotlin - String Templates](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
