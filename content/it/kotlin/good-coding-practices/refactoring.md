---
title:                "Rifattorizzazione"
aliases:
- /it/kotlin/refactoring/
date:                  2024-01-26T01:43:58.849283-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Il refactoring è il processo di modificare il codice esistente per migliorarne la struttura, la leggibilità e le prestazioni senza cambiarne il comportamento esterno. I programmatori fanno refactoring per rendere il codice più manutenibile, per semplificare l'aggiunta di nuove funzionalità e per individuare e correggere più facilmente i bug.

## Come fare:
Ecco un frammento di codice Kotlin che mostra un comune "bad smell" del codice e la sua versione rifattorizzata. Partiamo da un pezzo di codice che fa troppo:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("ID dell'ordine: ${order.id}")
        // Calcolo del totale dell'ordine
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Applica sconto
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Totale: $total")
        // Altre elaborazioni...
    }
}
```

Rifattorizzato per una migliore leggibilità e separazione delle responsabilità:

```kotlin
fun printOrderSummary(order: Order) {
    print("ID dell'ordine: ${order.id}")
    val total = calculateTotal(order)
    print("Totale: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Nessun output di esempio qui dato che non abbiamo cambiato la funzionalità, ma la leggibilità e la manutenibilità del codice hanno ricevuto un enorme impulso!

## Approfondimento
Il concetto di refactoring esiste da quando ha inizio la programmazione, ma ha realmente preso piede come disciplina negli anni '90, specialmente dopo che Martin Fowler ha pubblicato "Refactoring: Migliorare il disegno del codice esistente" nel 1999. Questo libro ha dato un nome alla pratica e ha definito un metodo organizzato per applicarla, inclusa una raccolta di tecniche di refactoring.

Confrontando il refactoring con le alternative: si potrebbe riscrivere il codice da zero (rischioso e dispendioso in termini di tempo), o semplicemente fare modifiche additive (porta a un aumento eccessivo del software e potenziale debito tecnico). Il refactoring colpisce il punto giusto: modernizza e pulisce mantenendo il rischio basso.

Per quanto riguarda l'implementazione, è essenziale avere un robusto insieme di test prima di iniziare il refactoring per assicurarsi di non cambiare accidentalmente il comportamento del programma. Molti IDE moderni (incluso IntelliJ per Kotlin) hanno strumenti di refactoring automatizzati per rinominare variabili, estrarre metodi e altro, che possono accelerare il processo e ridurre gli errori.

## Vedi anche
- "Refactoring: Migliorare il disegno del codice esistente" di Martin Fowler (per l'opera fondamentale su questo argomento)
- Documentazione Kotlin sulle convenzioni di codifica: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (per capire il 'modo Kotlin' di scrivere codice pulito)
- Supporto di JetBrains per il refactoring in IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (per l'uso pratico degli strumenti di refactoring)
- Guida di Google sul refactoring su larga scala: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (per intuizioni su come affrontare sfide di refactoring più ampie)
