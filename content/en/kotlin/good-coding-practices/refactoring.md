---
title:                "Refactoring"
aliases:
- /en/kotlin/refactoring/
date:                  2024-01-25T02:11:55.064319-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is the process of tweaking existing code to improve its structure, readability, and performance without changing its external behavior. Programmers refactor to make code more maintainable, to simplify the addition of new features, and to spot and fix bugs more easily.

## How to:
Here's a Kotlin snippet showing a common code smell and its refactored version. We start with a chunk of code that's doing too much:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Calculating order total
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Apply discount
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // More processing...
    }
}
```

Refactored for better readability and separation of concerns:

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

No sample output here since we didn't change the functionality, but the code readability and maintainability got a huge boost!

## Deep Dive
Refactoring as a concept has been around since programming began, but it really took off as a discipline in the 1990s, especially after Martin Fowler published "Refactoring: Improving the Design of Existing Code" in 1999. This book gave a name to the practice and defined an organized method for applying it, including a catalog of refactoring techniques.

Comparing refactoring to alternatives: you could rewrite code from scratch (risky and time-consuming), or simply make additive changes (leads to software bloat and potential tech debt). Refactoring hits the sweet spot—it modernizes and cleans up while keeping the risk low.

Implementation wise, it's essential to have a robust set of tests before you start refactoring to ensure you don't accidentally change the program's behavior. Many modern IDEs (including IntelliJ for Kotlin) have automated refactoring tools to rename variables, extract methods, and more, which can speed up the process and reduce errors.

## See Also
- "Refactoring: Improving the Design of Existing Code" by Martin Fowler (for the foundational work on this topic)
- Kotlin documentation on coding conventions: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (to understand the 'Kotlin way' of clean code)
- JetBrains support for refactoring in IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (for practical refactoring tool usage)
- Google's guide on refactoring at scale: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (for insights on tackling larger refactoring challenges)
