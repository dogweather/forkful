---
date: 2024-01-26 01:44:16.511826-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r ett Kotlin-utdrag som visar en vanlig kodlukt\
  \ och dess refaktoriserade version. Vi b\xF6rjar med en kodsnutt som g\xF6r f\xF6\
  r mycket."
lastmod: '2024-03-13T22:44:37.879570-06:00'
model: gpt-4-0125-preview
summary: "H\xE4r \xE4r ett Kotlin-utdrag som visar en vanlig kodlukt och dess refaktoriserade\
  \ version."
title: Omskrivning av kod
weight: 19
---

## Hur man gör:
Här är ett Kotlin-utdrag som visar en vanlig kodlukt och dess refaktoriserade version. Vi börjar med en kodsnutt som gör för mycket:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order-ID: ${order.id}")
        // Beräknar order total
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Tillämpar rabatt
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Totalt: $total")
        // Mer bearbetning...
    }
}
```

Refaktoriserad för bättre läsbarhet och separation av ansvar:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order-ID: ${order.id}")
    val total = calculateTotal(order)
    print("Totalt: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Inget exempel på utmatning här eftersom vi inte ändrade funktionaliteten, men kodens läsbarhet och underhållbarhet fick en enorm skjuts!

## Djupdykning
Refaktorisering som koncept har funnits så länge programmering har funnits, men det tog verkligen fart som en disciplin under 1990-talet, särskilt efter att Martin Fowler publicerade "Refaktorisering: Att förbättra designen på befintlig kod" år 1999. Den här boken gav praktiken ett namn och definierade en organiserad metod för att tillämpa den, inklusive en katalog med refaktoreringstekniker.

Jämfört med alternativen: du skulle kunna skriva om kod från grunden (riskabelt och tidskrävande), eller helt enkelt göra additiva förändringar (leder till mjukvarusvullnad och potentiell teknisk skuld). Refaktorisering träffar den gyllene medelvägen – den moderniserar och städar upp samtidigt som risken hålls låg.

När det gäller implementering är det viktigt att ha ett robust testset innan du börjar refaktorisera för att säkerställa att du inte av misstag ändrar programmets beteende. Många moderna IDEer (inklusive IntelliJ för Kotlin) har automatiserade refaktoreringsverktyg för att byta namn på variabler, extrahera metoder och mer, vilket kan påskynda processen och minska fel.

## Se även
- "Refaktorisering: Att förbättra designen på befintlig kod" av Martin Fowler (för grundläggande arbete på detta ämne)
- Kotlin-dokumentation om kodkonventioner: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (för att förstå 'Kotlin-sättet' för ren kod)
- JetBrains stöd för refaktorisering i IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (för praktisk användning av refaktoreringsverktyg)
- Googles guide till refaktorisering i stor skala: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (för insikter om att ta itu med större refaktoreringsutmaningar)
