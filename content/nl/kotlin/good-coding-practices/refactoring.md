---
title:                "Refactoring"
date:                  2024-01-28T22:06:12.024104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het aanpassen van bestaande code om de structuur, leesbaarheid, en prestatie te verbeteren zonder het externe gedrag te veranderen. Programmeurs refactoren om code beter onderhoudbaar te maken, het toevoegen van nieuwe functies te vereenvoudigen, en bugs makkelijker te vinden en op te lossen.

## Hoe:
Hier is een Kotlin snippet die een veelvoorkomende codegeur laat zien en de gerefactorde versie ervan. We beginnen met een stuk code dat te veel doet:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Totale bestelling berekenen
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Korting toepassen
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Totaal: $total")
        // Meer verwerking...
    }
}
```

Gerefactord voor betere leesbaarheid en scheiding van zorgen:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Totaal: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Geen voorbeeldoutput hier, aangezien we de functionaliteit niet hebben veranderd, maar de leesbaarheid en onderhoudbaarheid van de code hebben een enorme boost gekregen!

## Diepgaand
Refactoring als een concept bestaat al sinds het begin van het programmeren, maar het nam echt een vlucht als een discipline in de jaren 90, vooral nadat Martin Fowler "Refactoring: Improving the Design of Existing Code" in 1999 publiceerde. Dit boek gaf een naam aan de praktijk en definieerde een georganiseerde methode om het toe te passen, inclusief een catalogus van refactoring-technieken.

Refactoring vergelijken met alternatieven: je zou code vanaf nul kunnen herschrijven (riskant en tijdrovend), of simpelweg additieve wijzigingen maken (leidt tot software-bloat en mogelijke techschuld). Refactoring raakt de sweet spot - het moderniseert en ruimt op terwijl het risico laag blijft.

Wat implementatie betreft, is het essentieel om een robuuste set tests te hebben voordat je begint met refactoren om ervoor te zorgen dat je niet per ongeluk het gedrag van het programma wijzigt. Veel moderne IDE's (waaronder IntelliJ voor Kotlin) hebben geautomatiseerde refactoringtools om variabelen te hernoemen, methoden uit te pakken, en meer, wat het proces kan versnellen en fouten kan verminderen.

## Zie Ook
- "Refactoring: Improving the Design of Existing Code" door Martin Fowler (voor het fundamentele werk over dit onderwerp)
- Kotlin documentatie over coderingsconventies: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (om de 'Kotlin manier' van schone code te begrijpen)
- JetBrains ondersteuning voor refactoring in IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (voor praktisch gebruik van refactoringtools)
- Google's gids over refactoring op schaal: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (voor inzichten over het aanpakken van grotere refactoringuitdagingen)
