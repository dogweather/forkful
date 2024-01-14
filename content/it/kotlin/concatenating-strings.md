---
title:                "Kotlin: Unione di stringhe"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nella programmazione che consiste nell'unire diverse stringhe per formare una sola stringa. Questa tecnica è utile per la scrittura di codice più pulito e leggibile, oltre che per la manipolazione di dati e la creazione di output personalizzati.

## Come Fare

La concatenazione di stringhe in Kotlin è possibile attraverso l'utilizzo dell'operatore "+" o del metodo "plus()". Vediamo un esempio pratico:

```Kotlin
val nome = "Maria"
val saluto = "Ciao"

val messaggio = saluto + " " + nome + "!"
println(messaggio)

//Output: Ciao Maria!
```

È anche possibile utilizzare la sintassi string template di Kotlin, utilizzando il carattere "$" per indicare una variabile all'interno di una stringa. Vediamo come:

```Kotlin
val nome = "Maria"
val saluto = "Ciao"

val messaggio = "$saluto $nome!"
println(messaggio)

//Output: Ciao Maria!
```

In entrambi i casi, il risultato è lo stesso: la concatenazione delle due stringhe "Ciao" e "Maria" per formare la stringa "Ciao Maria!".

## Approfondimento

La concatenazione delle stringhe in Kotlin è spesso una delle prime operazioni che si impara nella programmazione, ma ci sono alcune cose da tenere in considerazione per evitare errori o problemi di performance.

Innanzitutto, è importante ricordare che le stringhe sono immutabili in Kotlin, quindi ogni volta che viene eseguita un'operazione di concatenazione, viene creata una nuova stringa. Questo significa che se si utilizzano molte stringhe e operazioni di concatenazione in un ciclo, si possono verificare rallentamenti nell'esecuzione del programma. In questi casi, è consigliabile utilizzare la classe StringBuilder, che consente la modifica della stringa senza dover crearne di nuove ogni volta.

Inoltre, è importante prestare attenzione all'ordine delle operazioni di concatenazione. Se nella stringa finale si desidera includere anche numeri, è consigliabile utilizzare il metodo toString() per convertire i numeri in stringhe, altrimenti verrebbero sommati ai valori invece di concatenarli.

## Vedi Anche

- Guida ufficiale di Kotlin sulla concatenazione di stringhe: https://kotlinlang.org/docs/basic-types.html#strings
- Tutorial su come utilizzare la classe StringBuilder in Kotlin: https://www.geeksforgeeks.org/kotlin-stringbuilder-class/
- Esempi pratici di concatenazione di stringhe in Kotlin: https://www.programiz.com/kotlin-programming/concatenate-strings