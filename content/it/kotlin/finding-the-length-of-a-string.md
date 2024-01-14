---
title:                "Kotlin: Trova la lunghezza di una stringa"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Perché
Trovare la lunghezza di una stringa è un'operazione comune nella programmazione. Conoscere la lunghezza di una stringa può essere utile per manipolare o visualizzare informazioni di testo.

##Come fare
Per trovare la lunghezza di una stringa in Kotlin, è possibile utilizzare il metodo `length()` come illustrato nell'esempio seguente:

```
Kotlin
val stringa = "Ciao mondo!"
println(stringa.length())

```
L'output di questo codice sarà `12`, poiché la stringa "Ciao mondo!" è composta da 12 caratteri.

##Deep Dive
Per comprendere meglio il metodo `length()`, è utile sapere come le stringhe sono rappresentate in Kotlin. In sostanza, una stringa è un oggetto che contiene una sequenza di caratteri. Il metodo `length()` restituisce il numero di caratteri presenti all'interno della stringa.

Inoltre, il metodo `length()` ha un tempo di esecuzione di O(1), il che significa che la sua complessità rimane costante indipendentemente dalla lunghezza della stringa. Questo lo rende un'operazione efficiente per determinare la lunghezza di una stringa anche con stringhe molto lunghe.

##Vedi anche
- Guida completa a Kotlin: https://www.raywenderlich.com/1940450-kotlin-tutorial-for-android-beginners
- Documentazione ufficiale di Kotlin: https://kotlinlang.org/docs/tutorials/getting-started.html