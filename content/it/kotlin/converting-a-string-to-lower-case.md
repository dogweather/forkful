---
title:                "Convertire una stringa in minuscolo"
html_title:           "Kotlin: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è e perché fare la conversione di una stringa in minuscolo?
La conversione di una stringa in minuscolo è un processo comune utilizzato dai programmatori per modificare una stringa inserita dall'utente in formato minuscolo. Questo è spesso utile quando si vuole eliminare le differenze tra le lettere maiuscole e minuscole in una stringa, ad esempio durante la ricerca o il confronto di dati.

## Come fare:
```
Kotlin
val stringa = "CIAO AMICI!"
println(stringa.toLowerCase())
// Output: ciao amici!
```

## Approfondimento:
Nel passato, molte lingue di programmazione non includevano una funzione per la conversione di una stringa in minuscolo, quindi i programmatori dovevano scrivere il proprio codice per farlo. Ora, con lo sviluppo di linguaggi di programmazione moderni, è diventato più semplice e veloce convertire una stringa in minuscolo utilizzando funzioni built-in come `toLowercase()` in Kotlin.

## Vedi anche:
- [Documentazione di Kotlin sulla conversione di stringhe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Esempi di codice per la conversione di stringhe in minuscolo in Kotlin](https://www.tutorialkart.com/kotlin/convert-string-to-lowercase-in-kotlin/)
- [Altri metodi per manipolare stringhe in Kotlin](https://www.baeldung.com/kotlin-string-manipulation)