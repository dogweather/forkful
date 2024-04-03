---
date: 2024-01-26 01:44:13.455403-07:00
description: "Jak to zrobi\u0107: Oto fragment kodu w Kotlinie pokazuj\u0105cy powszechny\
  \ \"smr\xF3d\" kodu oraz jego zrefaktoryzowan\u0105 wersj\u0119. Zaczynamy od kawa\u0142\
  ka kodu, kt\xF3ry robi za\u2026"
lastmod: '2024-03-13T22:44:35.373360-06:00'
model: gpt-4-0125-preview
summary: "Oto fragment kodu w Kotlinie pokazuj\u0105cy powszechny \"smr\xF3d\" kodu\
  \ oraz jego zrefaktoryzowan\u0105 wersj\u0119."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Oto fragment kodu w Kotlinie pokazujący powszechny "smród" kodu oraz jego zrefaktoryzowaną wersję. Zaczynamy od kawałka kodu, który robi za dużo:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Obliczanie sumy zamówienia
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Zastosowanie rabatu
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Więcej przetwarzania...
    }
}
```

Zrefaktoryzowany dla lepszej czytelności i oddzielenia odpowiedzialności:

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

Nie ma tutaj przykładowego wyniku, ponieważ nie zmieniliśmy funkcjonalności, ale czytelność i łatwość utrzymania kodu znacząco wzrosły!

## Dogłębna analiza
Refaktoryzacja jako koncepcja istnieje od początków programowania, ale naprawdę zyskała na znaczeniu jako dyscyplina w latach 90., szczególnie po publikacji przez Martina Fowlera "Refaktoryzacja: Udoskonalanie struktury istniejącego kodu" w 1999 roku. Ta książka dała nazwę praktyce i zdefiniowała zorganizowaną metodę jej stosowania, w tym katalog technik refaktoryzacji.

Porównując refaktoryzację do alternatyw: można napisać kod od nowa (ryzykowne i czasochłonne) lub po prostu dokonywać dodatkowych zmian (prowadzi do nadmiaru oprogramowania i potencjalnego długu technicznego). Refaktoryzacja trafia w słodki punkt - modernizuje i porządkuje, jednocześnie utrzymując ryzyko na niskim poziomie.

Jeśli chodzi o implementację, kluczowe jest posiadanie solidnego zestawu testów przed rozpoczęciem refaktoryzacji, aby upewnić się, że nie zmienisz przypadkowo zachowania programu. Wiele nowoczesnych środowisk IDE (w tym IntelliJ dla Kotlina) posiada zautomatyzowane narzędzia do refaktoryzacji, takie jak zmiana nazw zmiennych, ekstrakcja metod i inne, które mogą przyspieszyć proces i zredukować błędy.

## Zobacz również
- "Refaktoryzacja: Udoskonalanie projektu istniejącego kodu" autorstwa Martina Fowlera (za podstawowe opracowanie na ten temat)
- Dokumentacja Kotlina na temat konwencji kodowania: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (aby zrozumieć "kotlinowy" sposób na czysty kod)
- Wsparcie JetBrains dla refaktoryzacji w IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (dla praktycznego użycia narzędzi do refaktoryzacji)
- Przewodnik Google’a na temat refaktoryzacji na dużą skalę: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (dla wglądu w radzenie sobie z większymi wyzwaniami refaktoryzacyjnymi)
