---
title:    "Kotlin: Capitalizzare una stringa"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molti programmi richiedono l'utilizzo di stringhe capitalizzate per scopi di formattazione o di ordinamento dei dati. In Kotlin, esiste una funzione built-in per capitalizzare una stringa, che può semplificare il processo di programmazione.

## Come fare

Per capitalizzare una stringa in Kotlin, useremo la funzione `capitalize()`. Prendiamo ad esempio la seguente stringa:

```Kotlin
val frase = "ciao a tutti"
```

Per capitalizzare la prima lettera di questa stringa, possiamo semplicemente chiamare la funzione `capitalize()` su di essa:

```
println(frase.capitalize())
// Output: Ciao a tutti
```

Possiamo anche capitalizzare tutte le parole della stringa utilizzando la funzione `capitalizeWords()`:

```Kotlin
val frase = "ciao a tutti"
println(frase.capitalizeWords())
// Output: Ciao A Tutti
```

Se desideriamo capitalizzare la prima lettera di ogni parola, ignorando eventuali lettere già maiuscole, possiamo utilizzare la funzione `capitalizeEachWord()`:

```Kotlin
val frase = "HO UN GATTO NEL GIARDINO"
println(frase.capitalizeEachWord())
// Output: Ho Un Gatto Nel Giardino
```

## Approfondimento

Quando si utilizza la funzione `toLowerCase()` su una stringa, solo la prima lettera di quella stringa verrà capitalizzata. Tutti gli altri caratteri rimarranno invariati. Per capitalizzare l'intera stringa, è necessario utilizzare la funzione `capitalize()`.

Inoltre, la funzione `capitalize()` non modifica la stringa originale, ma ne crea una nuova con la prima lettera in maiuscolo. Se si desidera modificare la stringa originale, si può assegnare il risultato della funzione ad essa:

```Kotlin
var frase = "ciao a tutti"
frase = frase.capitalize()
println(frase)
// Output: Ciao a tutti
```

È importante tenere presente che la funzione `capitalize()` considera solo il primo carattere della stringa e ignora gli eventuali caratteri speciali o numeri. Ad esempio, se la prima lettera della stringa è un numero o un carattere speciale, non verrà maiuscolato.

## Vedi anche

- [Documentazione ufficiale di Kotlin sulle operazioni di stringhe](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial di programmazione Kotlin su manipolazione di stringhe](https://kotlinlang.org/docs/basic-syntax.html#strings)
- [Articolo su Medium sull'utilizzo di funzioni di stringhe in Kotlin](https://medium.com/android-things/a-b-c-you-would-probably-need-to-use-lowercase-uppercase-capitalize-5f84c2af2a2d)