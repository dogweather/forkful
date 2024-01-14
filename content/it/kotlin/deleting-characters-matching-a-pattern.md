---
title:                "Kotlin: Eliminazione di caratteri corrispondenti a un modello."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Nella programmazione, ci possono essere diverse ragioni per voler eliminare dei caratteri che corrispondono ad un certo pattern. Ad esempio, può essere utile per ripulire dati di input o per modificare una stringa in un formato specifico.

## Come Fare
Per eliminare i caratteri che corrispondono ad un pattern in Kotlin, possiamo utilizzare la funzione `replace` sulla stringa di input. Questa funzione prende come parametri il pattern da cercare e la stringa di sostituzione.

```Kotlin
val input = "123-456-789"
val output = input.replace(Regex("[^\\d]"), "") // output: 123456789
```

Nell'esempio sopra, stiamo eliminando tutti i caratteri non numerici dalla stringa di input utilizzando un'espressione regolare. Possiamo anche utilizzare la funzione `replace` per sostituire i caratteri con un altro valore, come mostrato nell'esempio seguente.

```Kotlin
val input = "abc123def"
val output = input.replace(Regex("[a-z]"), "X") // output: XXX123XXX
```

## Approfondimento
L'uso delle espressioni regolari può essere molto utile quando si tratta di rimuovere caratteri che corrispondono ad un determinato pattern. Le espressioni regolari sono uno strumento potente per la manipolazione dei dati e possono essere utilizzate in diverse situazioni di programmazione. Per saperne di più sull'utilizzo di espressioni regolari in Kotlin, si consiglia di consultare la documentazione ufficiale.

## Vedi Anche
- [Documentazione ufficiale di Kotlin sulle espressioni regolari](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutorial per principianti sulle espressioni regolari in Kotlin](https://www.raywenderlich.com/2637734-kotlin-regular-expressions-tutorial-getting-started)