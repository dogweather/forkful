---
title:    "Kotlin: Trova la lunghezza di una stringa"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Una delle operazioni più comuni nella programmazione è trovare la lunghezza di una stringa. Questa informazione ci permette di manipolare e analizzare le stringhe in modo più efficiente. In questo articolo, vedremo come trovare la lunghezza di una stringa utilizzando Kotlin.

## Come fare

Per trovare la lunghezza di una stringa in Kotlin, possiamo utilizzare il metodo `length()`. Questo metodo restituisce il numero di caratteri presenti nella stringa.

```Kotlin
val stringa = "Ciao mondo!"
println(stringa.length()) // Output: 11 (spazi inclusi)
```

Come possiamo vedere dall'esempio, utilizziamo il metodo `length()` sull'oggetto stringa e stampiamo il risultato. Se la stringa contiene degli spazi, questi verranno conteggiati nella lunghezza totale.

Possiamo anche utilizzare il metodo `length` su una variabile di tipo `String?`. In questo caso, il metodo restituirà `null` se la variabile non è inizializzata.

```Kotlin
var stringa2: String? = null
println(stringa2.length) // Output: null
```

## Approfondimento

Per capire meglio come funziona il metodo `length()`, dobbiamo conoscere un po' di informazioni sulle stringhe in Kotlin. In Kotlin, una stringa è un oggetto immutabile, il che significa che una volta che viene creata, non può essere modificata. Questo perché ogni volta che apportiamo una modifica alla stringa, Kotlin crea un nuovo oggetto stringa invece di modificare quello esistente.

Il metodo `length()` è un metodo della classe `String`, che rappresenta una stringa in Kotlin. Questa classe ha anche altri metodi utili per manipolare e analizzare le stringhe.

Ora che abbiamo una migliore comprensione di come funziona il metodo `length()`, possiamo utilizzarlo in combinazione con altre funzioni per ottenere l'esatto numero di caratteri che ci interessano, come ad esempio eliminare gli spazi bianchi o contare solo le lettere.

## Vedi anche

- [Documentazione ufficiale di Kotlin sulle stringhe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Tutorial su come utilizzare le stringhe in Kotlin](https://www.geeksforgeeks.org/kotlin-string/)
- [Esempi pratici su come manipolare le stringhe in Kotlin](https://blog.kotlin-academy.com/5-library-functions-for-manipulating-strings-in-kotlin-afe7ed1af0c0)