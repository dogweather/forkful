---
title:                "Estrazione di sottostringhe"
html_title:           "Kotlin: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Spesso può essere necessario e utile estrarre parti specifiche di una stringa all'interno di un programma. In questo articolo, vedremo come fare ciò utilizzando Kotlin in modo semplice e veloce.

## Come fare

Per estrarre una parte di una stringa in Kotlin, possiamo utilizzare il metodo `substring()` che accetta due parametri: l'indice iniziale e l'indice finale della sottostringa che vogliamo estrarre. Ad esempio, se abbiamo una stringa "Ciao Mondo" e vogliamo estrarre solo la parola "Mondo", possiamo usare il metodo `substring(5, 10)` dove 5 è l'indice iniziale della parola "Mondo" e 10 è l'indice finale (notare che l'indice finale non verrà incluso nella sottostringa).

Possiamo anche utilizzare il metodo `drop()` per eliminare le prime n lettere della stringa e poi utilizzare il metodo `take()` per prendere le prossime m lettere. Ad esempio, se abbiamo una stringa "Ciao Mondo" e vogliamo eliminare la parola "Ciao" e prendere solo la parola "Mondo", possiamo usare il metodo `drop(5).take(5)`. Questo ci darà una sottostringa di 5 lettere a partire dal sesto carattere della stringa originale.

Di seguito un esempio di codice completo:

```Kotlin
fun main() {
   val str = "Ciao Mondo"
   val subStr1 = str.substring(5, 10)
   val subStr2 = str.drop(5).take(5)
  
   println(subStr1) // Output: Mondo
   println(subStr2) // Output: Mondo
}
```

## Deep Dive

Kotlin offre anche altri metodi per estrarre sottostringhe, come ad esempio `slice()` che permette di estrarre più sottostringhe specificando gli indici desiderati, oppure `subSequence()` che restituisce una sequenza dei caratteri della sottostringa indicata. Inoltre, è possibile utilizzare espressioni regolari per estrarre sottostringhe più complesse e utilizzare il metodo `replace()` per sostituire una sottostringa nella stringa originale.

Vale la pena notare che Kotlin considera tutte le stringhe come sequenze di caratteri unicode, quindi è possibile utilizzare i metodi sopra menzionati anche per estrarre sottostringhe di caratteri speciali.

## Vedi anche

- [Documentazione ufficiale di Kotlin sulle stringhe](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlin String extensions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.-string/index.html)