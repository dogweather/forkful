---
title:                "Kotlin: Convertire una stringa in minuscolo"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui potresti voler convertire una stringa in caratteri minuscoli in Kotlin. Potresti aver bisogno di confrontare due stringhe in modo da ignorare le differenze di maiuscole e minuscole oppure potresti essere interessato ad ottenere un output uniforme in minuscolo per formattare il testo. Qualunque sia il motivo, il processo di conversione in minuscolo è fondamentale per molte operazioni di manipolazione delle stringhe.

## Come fare

Per convertire una stringa in minuscolo, puoi utilizzare il metodo `toLowerCase()` sulla stringa desiderata. Ad esempio:

```kotlin
fun main() {
    val stringa = "Ciao Mondo"
    val nuovaStringa = stringa.toLowerCase()
    println(nuovaStringa)
}
```

In questo caso, l'output sarà "ciao mondo", con tutti i caratteri convertiti in minuscolo. 

Puoi anche utilizzare il metodo `toLowerCase()` su una sottostringa di una stringa più grande, specificando gli indici di inizio e fine della sottostringa come parametri del metodo.

```kotlin
fun main() {
    val stringa = "ABCDEF"
    val nuovaStringa = stringa.toLowerCase(1, 4)
    println(nuovaStringa)
}
```

In questo caso, l'output sarà "AbcdEF", in quanto solo i caratteri compresi tra l'indice 1 (incluso) e l'indice 4 (escluso) saranno convertiti in minuscolo.

## Approfondimento

E' importante notare che il metodo `toLowerCase()` crea una nuova stringa con la conversione in minuscolo, senza modificare la stringa originale. Inoltre, la conversione dei caratteri in minuscolo dipende dalla lingua in uso, in quanto alcune lingue hanno regole specifiche per la conversione dei caratteri. Ad esempio, nella lingua tedesca la lettera "ß" diventa "ss" quando convertita in minuscolo.

È possibile utilizzare anche il metodo `toLowerCase(Locale)` per specificare la lingua desiderata per la conversione dei caratteri in minuscolo.

```kotlin
fun main() {
    val stringa = "ß"
    val nuovaStringa = stringa.toLowerCase(Locale.GERMAN)
    println(nuovaStringa) // output: "ss"
}

```

Inoltre, è importante prestare attenzione alla codifica dei caratteri nella stringa quando si effettua la conversione in minuscolo, in modo da evitare problemi di compatibilità.

## Vedi anche

- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial su come manipolare le stringhe in Kotlin](https://www.programiz.com/kotlin-programming/strings)
- [Esempi di utilizzo del metodo `toLowerCase()`](https://www.tutorialkart.com/kotlin/string-tolowercase-method-example/#:~:text=The%20Kotlin%20String.toLowerCase(),returns%20new%20String%20in%20lowercase.)