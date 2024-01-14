---
title:    "Kotlin: Scrivere su errori standard"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error è un'attività importante per gli sviluppatori di Kotlin. Permette di identificare e risolvere facilmente eventuali errori all'interno del codice, migliorando la qualità del software.

## Come fare

Per scrivere a standard error in Kotlin, è necessario utilizzare la funzione "err" della classe "java.lang.System". Ecco un esempio di codice: 

```Kotlin
fun main() {
    System.err.println("Errore!")
}
```

Il codice sopra stampa "Errore!" sullo standard error. Ogni volta che si incontrano errori durante l'esecuzione del programma, questi verranno mostrati sullo standard error. Vediamo un altro esempio: 

```Kotlin
fun main() {
    val num1 = 10
    val num2 = 0
    try {
        val result = num1/num2
        System.err.println("Il risultato è: $result")
    } catch (e: Exception) {
        System.err.println("Errore durante la divisione per zero")
    }
}
```

In questo caso, il programma proverà a dividere 10 per 0 e, poiché questa è un'operazione impossibile, verrà sollevata un'eccezione. La stringa "Errore durante la divisione per zero" verrà stampata sullo standard error.

## Approfondimento

Scrivere a standard error è particolarmente utile per identificare e gestire gli errori in modo efficiente. Quando si scrive a standard error, si utilizza la classe "java.lang.System", che fornisce anche altre funzioni utili come "out", "in" e "currentTimeMillis". Inoltre, utilizzare la funzione "err" aiuta a mantenere il codice più leggibile e organizzato.

## Vedi anche
- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)
- [Tutorial su come gestire gli errori in Kotlin](https://www.techotopia.com/index.php/Kotlin_Exception_Handling_-_throw,_throws,_try,_catch_and_finally)