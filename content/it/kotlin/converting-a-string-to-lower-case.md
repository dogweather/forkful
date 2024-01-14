---
title:    "Kotlin: Convertire una stringa in minuscolo"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe voler convertire una stringa in caratteri minuscoli. Ad esempio, potrebbe essere necessario per eseguire confronti di stringhe sensibili alle maiuscole e minuscole o per formattare correttamente i dati in un database.

## Come fare

Convertire una stringa in minuscolo è un'operazione molto semplice in Kotlin. Basta utilizzare il metodo `toLowerCase()` su una variabile di tipo stringa. Di seguito è riportato un esempio di codice che mostra come farlo:

```Kotlin
val stringa = "QuestA è Una StriNGa Da COmVeRtIre"
val stringaMinuscola = stringa.toLowerCase()
println(stringaMinuscola)
```
**Output:**
```
questa è una stringa da convertire
```

È importante notare che questo metodo restituisce una nuova stringa convertita in minuscolo, senza modificare la variabile originale. Inoltre, è possibile passare una locale specifica come parametro del metodo per gestire le diverse convenzioni di maiuscole e minuscole in diverse lingue.

## Approfondimento

Oltre al semplice utilizzo del metodo `toLowerCase()`, ci sono altre considerazioni da tenere a mente quando si lavora con stringhe in Kotlin. Per esempio, è importante ricordare che le stringhe sono immutabili, il che significa che non possono essere modificate direttamente. Quindi, quando si esegue una conversione di stringa, in realtà si sta creando una nuova stringa modificata.

Inoltre, c'è anche la possibilità di utilizzare le espressioni regolari per sostituire tutte le occorrenze di una lettera maiuscola con la corrispondente lettera minuscola in una stringa. Ci sono vari metodi disponibili in Kotlin per manipolare le stringhe, quindi è consigliabile esplorare la documentazione per trovare la soluzione più adatta alle proprie esigenze.

## Vedi anche
- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Guida introduttiva a Kotlin per programmatori Java](https://kotlinlang.org/docs/reference/using-gradle.html)
- [Tutorial su espressioni regolari in Kotlin](https://www.baeldung.com/kotlin/regex)