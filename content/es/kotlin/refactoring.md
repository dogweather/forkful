---
title:                "Refactorización"
date:                  2024-01-26T01:42:45.969008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Refactoring es el proceso de ajustar código existente para mejorar su estructura, legibilidad, y rendimiento sin cambiar su comportamiento externo. Los programadores hacen refactoring para hacer el código más mantenible, para simplificar la adición de nuevas características y para localizar y arreglar errores más fácilmente.

## Cómo:
Aquí tienes un fragmento en Kotlin que muestra un olor común del código y su versión refactorizada. Comenzamos con un bloque de código que está haciendo demasiado:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Calculando el total de la orden
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Aplicar descuento
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Más procesamiento...
    }
}
```

Refactorizado para mejorar la legibilidad y la separación de preocupaciones:

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

Aquí no hay muestra de salida ya que no cambiamos la funcionalidad, ¡pero la legibilidad y mantenibilidad del código recibieron un gran impulso!

## Profundización
El refactoring como concepto existe desde que comenzó la programación, pero realmente despegó como una disciplina en la década de 1990, especialmente después de que Martin Fowler publicara "Refactoring: Mejorando el Diseño de Código Existente" en 1999. Este libro le dio un nombre a la práctica y definió un método organizado para aplicarlo, incluyendo un catálogo de técnicas de refactoring.

Comparando el refactoring con alternativas: podrías reescribir el código desde cero (arriesgado y consume mucho tiempo), o simplemente hacer cambios adicionales (conduce a inflación del software y potencial deuda técnica). El refactoring encuentra el punto óptimo: moderniza y limpia manteniendo el riesgo bajo.

En términos de implementación, es esencial tener un conjunto robusto de pruebas antes de comenzar a refactorizar para asegurarte de no cambiar accidentalmente el comportamiento del programa. Muchos IDEs modernos (incluido IntelliJ para Kotlin) tienen herramientas de refactoring automatizadas para renombrar variables, extraer métodos y más, lo que puede acelerar el proceso y reducir errores.

## Ver También
- "Refactoring: Mejorando el Diseño de Código Existente" por Martin Fowler (para el trabajo fundacional sobre este tema)
- Documentación de Kotlin sobre convenciones de codificación: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (para entender la 'manera Kotlin' de código limpio)
- Soporte de JetBrains para refactoring en IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (para el uso práctico de herramientas de refactoring)
- Guía de Google sobre el refactoring a gran escala: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (para obtener perspectivas sobre cómo abordar desafíos de refactoring más grandes)
