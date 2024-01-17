---
title:                "Generazione di numeri casuali"
html_title:           "Kotlin: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Generare numeri casuali è una tecnica comune utilizzata dai programmatori per aggiungere casualità a un programma o al gioco che stanno sviluppando. Questo processo è comunemente utilizzato in giochi, lotterie e altri scenari in cui è necessario ottenere un risultato casuale. 

## Come fare:
Per generare numeri casuali in Kotlin, è possibile utilizzare le funzioni integrate fornite dalla libreria standard di Kotlin. Ecco un esempio di come creare un numero casuale compreso tra 1 e 10 utilizzando la funzione `random()`:

```Kotlin
val randomNumber = (1..10).random()
println(randomNumber)
```

Questo codice genererà un numero casuale come output, che potrebbe essere ad esempio 5. Si possono anche specificare limiti diversi per ottenere numeri casuali in un determinato intervallo, ad esempio `(50..100).random()` genererà un numero casuale compreso tra 50 e 100.

## Approfondimento:
I programmatori hanno sempre avuto la necessità di generare numeri casuali nei loro programmi, soprattutto in applicazioni ludiche. In passato, questa funzionalità richiedeva l'utilizzo di complessi algoritmi matematici. Oggi, grazie alle funzioni integrate delle librerie standard, è diventato molto più semplice e veloce includere numeri casuali nei nostri programmi.

## Vedi anche:
- Documentazione ufficiale sulla generazione di numeri casuali in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html
- Altro articolo su come usare la funzione `random()` in Kotlin: https://developer.android.com/training/basics/random