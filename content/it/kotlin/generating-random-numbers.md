---
title:    "Kotlin: Generazione di numeri casuali"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'abilità fondamentale per qualsiasi sviluppatore di software. I numeri casuali sono utilizzati in una varietà di applicazioni, come giochi, algoritmi di crittografia, simulazioni e test di software. Imparare come generare numeri casuali in Kotlin ti aiuterà ad aggiungere un elemento di casualità ai tuoi progetti e a rendere il tuo codice più dinamico e interessante.

## Come fare

Ci sono diversi modi per generare numeri casuali in Kotlin. Uno dei modi più semplici è utilizzare la classe Random di Java. Ecco come puoi farlo:

```
Kotlin
val random = Random()
val number = random.nextInt()
println(number)
// Output: -58327484
```

In questo esempio, abbiamo creato un'istanza della classe Random e abbiamo utilizzato il metodo nextInt() per generare un intero casuale. Il valore verrà stampato a video ogni volta che esegui il codice, producendo un risultato diverso ogni volta.

Se vuoi generare un numero casuale in un intervallo specifico, puoi utilizzare il seguente codice:

```
Kotlin
val random = Random()
val number = random.nextInt(10) // Genera un numero tra 0 e 9
println(number)
// Output: 4
```

Qui, abbiamo utilizzato il metodo nextInt() per generare un intero tra 0 e 9. Se vuoi generare un numero tra due estremi specifici, puoi utilizzare il seguente codice:

```
Kotlin
val random = Random()
val number = random.nextInt(5) + 10 // Genera un numero tra 10 e 14
println(number)
// Output: 12
```

In questo esempio, abbiamo utilizzato il metodo nextInt() per generare un intero casuale tra 0 e 4, e poi abbiamo aggiunto 10 al risultato, ottenendo così un numero tra 10 e 14.

## Approfondimento

Quando si tratta di generare numeri casuali, è importante capire che non esiste un numero veramente casuale. Tutti i generatori di numeri casuali si basano su un algoritmo che utilizza un numero precedente o un valore iniziale come base per generare il numero successivo.

Inoltre, è importante utilizzare una sorgente di entropia (casualità) per inizializzare il generatore di numeri casuali, altrimenti i numeri generati saranno sempre gli stessi. In Kotlin, questo viene fatto automaticamente dalla classe Random utilizzando la sorgente di entropia del sistema operativo.

Un'altra cosa da considerare è che i generatori di numeri casuali seguono un modello predeterminato, quindi è possibile che alcuni numeri vengano generati più frequentemente di altri. Se vuoi generare numeri completamente casuali, puoi utilizzare un servizio esterno che offre veri e propri numeri casuali basati su fenomeni naturali come il decadimento radioattivo.

## Vedi anche
- Documentazione sulla classe Random: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-random/index.html
- Generare numeri casuali in Java: https://www.baeldung.com/java-generate-random-long-float-integer-double
- Servizi di numeri casuali online: https://www.random.org/