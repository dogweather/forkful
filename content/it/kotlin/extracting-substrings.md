---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:46:05.439787-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Estrarre sottosequenze significa prendere pezzi di una stringa. Gli sviluppatori lo fanno per analizzare i dati, pulire input o solo per estrarre le parti rilevanti di testo.

## Come fare:
```Kotlin
fun main() {
    val frase = "Ciao mondo Kotlin!"
    val saluto = frase.substring(0, 4)
    val oggettoSaluto = frase.substring(5, 10)
    
    println(saluto) // Output: Ciao
    println(oggettoSaluto) // Output: mondo
}
```

## Approfondimento
Kotlin, come Java, ha una funzione `substring` che viene dal mondo del C e C++ con le loro funzioni `strncpy` e `substr`. I metodi alternativi includono l'uso di `split`, regex o le API di Kotlin `take` e `drop`. La funzione `substring` di Kotlin è sicura – se l'indice è fuori dai limiti, otterrai un'eccezione e in questo modo, il controllo degli errori è integrato.

## Vedere anche
- Documentazione ufficiale Kotlin sulla funzione [substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- Kotlin API reference per [`split`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/split.html)
- Post sul blog su [manipolazione di stringhe in Kotlin](https://blog.kotlin-academy.com/mastering-kotlin-strings-2da3e7ccfd14)