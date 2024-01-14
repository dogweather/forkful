---
title:    "Kotlin: Trova la lunghezza di una stringa"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui potresti voler sapere la lunghezza di una stringa quando stai scrivendo codice in Kotlin. Ad esempio, potresti voler convalidare l'input dell'utente, manipolare le stringhe in base alla loro lunghezza o semplicemente visualizzare la lunghezza di una stringa come parte di un output.

## Come farlo

Per trovare la lunghezza di una stringa in Kotlin, puoi utilizzare il metodo `length()` che è disponibile per tutte le stringhe. Ecco un esempio di codice che mostra come utilizzarlo:

```Kotlin
val str = "Ciao mondo!"
println(str.length()) // Output: 11
```

Come puoi vedere, il metodo `length()` restituisce semplicemente il numero di caratteri presenti nella stringa. In questo caso, ci sono 11 caratteri in "Ciao mondo!", quindi l'output è 11.

È anche possibile utilizzare il metodo `length` su una variabile di tipo `String?`. In quel caso, restituirà la lunghezza della stringa solo se la variabile non è nulla.

```Kotlin
val str: String? = null
println(str?.length) // Output: null
```

In questo esempio, la variabile è nulla, quindi non è presente alcuna stringa su cui chiamare il metodo `length()`, il che significa che l'output è `null`. Assicurati di gestire correttamente questi casi nelle tue applicazioni per evitare errori.

## Approfondimento

Un'interessante caratteristica del metodo `length()` è che non è necessariamente lineare rispetto alla lunghezza della stringa. Ad esempio, quando si esegue questo codice:

```Kotlin
val str = "ooooooo"
println(str.length()) // Output: 7
```

Si potrebbe pensare che l'output sia 8, dato che ci sono 8 caratteri nella stringa, ma in realtà è 7. Questo perché Kotlin utilizza una rappresentazione interna della stringa che tiene conto di caratteri speciali e combinazioni di caratteri, il che significa che la lunghezza potrebbe differire dal numero di caratteri effettivamente presenti.

Inoltre, in Kotlin è possibile utilizzare l'operatore `size` su una stringa, che è equivalente al metodo `length()`. Tuttavia, non è consigliato utilizzarlo, poiché potrebbe portare a equivoci con la funzione `size()` utilizzata per le collezioni.

## Vedi anche

- [Metodo length() della classe String di Kotlin (in inglese)](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Tutorial di Kotlin su stringhe (in italiano)](https://www.androhub.com/kotlin-string/)
- [Documentazione ufficiale di Kotlin sulla gestione delle stringhe (in inglese)](https://kotlinlang.org/docs/basic-syntax.html#strings)