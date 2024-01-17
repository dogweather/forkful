---
title:                "Unire stringhe"
html_title:           "Kotlin: Unire stringhe"
simple_title:         "Unire stringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La concatenazione di stringhe è un'operazione comune in programmazione che consiste nell'unire più stringhe per formare una nuova stringa. I programmatori lo fanno per costruire messaggi, log e qualsiasi altra cosa che richieda l'unione di testo.

## Come si fa:
Per concatenare le stringhe in Kotlin, è possibile utilizzare l'operatore "plus" (+) o il metodo "plus()" delle stringhe. Ecco un esempio di entrambi i metodi:

```Kotlin
// Con l'operatore "+":
val str1 = "Hello"
val str2 = "World"
val str3 = str1 + " " + str2 // str3 diventa "Hello World"
println(str3)

// Con il metodo "plus()":
val str4 = "Kotlin"
val str5 = "is awesome"
val str6 = str4.plus(" ").plus(str5) // str6 diventa "Kotlin is awesome"
println(str6)
```

L'output di questo codice sarebbe:

```
Hello World
Kotlin is awesome
```

## Approfondimento:
La concatenazione di stringhe è stata una delle prime operazioni del linguaggio di programmazione BASIC negli anni '60. Nei tempi moderni, ci sono anche altre alternative per unire le stringhe come l'uso del metodo "format()" o l'uso delle funzioni "StringBuilder" o "StringBuffer" per migliorare le prestazioni quando si lavora con molte stringhe.

## Vedi anche:
- Documentazione ufficiale di Kotlin sull'operazione di concatenazione di stringhe: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/plus.html
- Un tutorial su come concatenare stringhe in Kotlin: https://www.programiz.com/kotlin-programming/string-concatenation