---
title:                "Refaktorering"
aliases:
- /no/kotlin/refactoring.md
date:                  2024-01-26T01:44:02.586424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorering er prosessen med å justere eksisterende kode for å forbedre dens struktur, lesbarhet og ytelse uten å endre dens eksterne oppførsel. Programmerere refaktorerer for å gjøre kode mer vedlikeholdbar, for å forenkle tillegg av nye funksjoner, og for å oppdage og fikse feil lettere.

## Hvordan:
Her er et Kotlin-utdrag som viser et vanlig kodeproblem og den refaktorerte versjonen. Vi starter med en kodebit som gjør for mye:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Beregner totalen for bestillingen
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Bruker rabatt
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Mer behandling...
    }
}
```

Refaktorert for bedre lesbarhet og separasjon av bekymringer:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Ingen eksempelutdata her siden vi ikke endret funksjonaliteten, men kodens lesbarhet og vedlikeholdbarhet fikk en enorm økning!

## Dypdykk
Refaktorering som et konsept har vært rundt siden programmering begynte, men det virkelig tok av som en disiplin på 1990-tallet, spesielt etter Martin Fowler publiserte "Refactoring: Improving the Design of Existing Code" i 1999. Denne boken ga en navn til praksisen og definerte en organisert metode for å anvende den, inkludert en katalog av refaktoreringsteknikker.

Sammenligner refaktorering med alternativer: du kunne skrive om kode fra bunnen av (risikabelt og tidkrevende), eller simpelthen gjøre tilleggsendringer (fører til programvareoppblåsning og potensiell teknisk gjeld). Refaktorering treffer det søte punktet – det moderniserer og rydder opp samtidig som risikoen holdes lav.

Når det gjelder implementering, er det avgjørende å ha et solid sett med tester før du begynner å refaktorere for å sikre at du ikke ved et uhell endrer programmets oppførsel. Mange moderne IDEer (inkludert IntelliJ for Kotlin) har automatiserte refaktoreringsverktøy for å omdøpe variabler, trekke ut metoder og mer, noe som kan fremskynde prosessen og redusere feil.

## Se også
- "Refactoring: Improving the Design of Existing Code" av Martin Fowler (for det grunnleggende arbeidet om dette emnet)
- Kotlin-dokumentasjon om kodestandarder: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (for å forstå den 'Kotlin-måten' på ren kode)
- JetBrains-støtte for refaktorering i IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (for praktisk bruk av refaktoreringsverktøy)
- Googles guide til refaktorering i stor skala: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (for innsikt i å takle større refaktoreringutfordringer)
