---
title:                "Kotlin: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Concatenare stringhe è un'operazione fondamentale nella programmazione, che permette di unire più stringhe in un'unica stringa. Questo può risultare utile in diverse situazioni, come ad esempio la costruzione di un output basato su input utente o la creazione di un URL dinamico.

## Come fare
Per concatenare stringhe in Kotlin, si può utilizzare l'operatore `+` o il metodo `.plus()`. Vediamo un esempio di entrambi i casi utilizzando due variabili stringa inizializzate con il valore "ciao" e "mondo":

```
Kotlin val string1 = "ciao"
val string2 = "mondo"

// Utilizzo dell'operatore +
val risultato1 = string1 + string2
println(risultato1) // Output: ciaomondo

// Utilizzo del metodo .plus()
val risultato2 = string1.plus(string2)
println(risultato2) // Output: ciaomondo
```

In entrambi i casi, si ottiene lo stesso risultato, ovvero la stringa "ciaomondo". Tuttavia, è importante notare che il metodo `.plus()` può essere utilizzato anche per concatenare più di due stringhe, come mostrato nell'esempio seguente:

```
Kotlin val string1 = "ciao"
val string2 = "a"
val string3 = "tutti"

// Utilizzo del metodo .plus() con più stringhe
val risultato = string1.plus(string2).plus(string3)
println(risultato) // Output: ciaoa tutti
```

## Approfondimento
È possibile concatenare non solo stringhe, ma anche altri tipi di dato, come ad esempio interi o booleani. In questo caso, Kotlin convertirà automaticamente il valore in una stringa e poi lo concatenerà. Vediamo un esempio:

```
Kotlin val numero = 42
val stringa = "Il numero è "

val risultato = stringa + numero
println(risultato) // Output: Il numero è 42
```

Inoltre, è possibile utilizzare la funzione `StringBuilder()` per concatenare più stringhe in modo più efficiente. Questo oggetto permette di unire le varie stringhe senza dover ogni volta crearne una nuova. Vediamo un esempio:

```
Kotlin val string1 = "ciao"
val string2 = "mondo"

val risultato = StringBuilder(string1).append(string2)
println(risultato.toString()) // Output: ciaomondo
```

## See Also
- [Documentazione ufficiale Kotlin su concatenazione di stringhe](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Esempi pratici di concatenazione di stringhe in Kotlin](https://github.com/kotlin/ktor/blob/f16f114a258a7b1ef209f2375e8e3345d00d3d2d/core/kotlinx-metadata/src/io/kotlinx/metadata/kotlin/KotlinClassMetadata.kt#L202)