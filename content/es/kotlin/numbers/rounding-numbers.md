---
title:                "Redondeo de números"
date:                  2024-01-26T03:45:33.904575-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Redondear números significa ajustarlos al número entero más cercano o a un grado de precisión especificado. Los programadores lo hacen para mejorar la legibilidad, reducir los requisitos de almacenamiento o porque el valor exacto no es crítico para los cálculos subsiguientes.

## Cómo hacerlo:

En Kotlin, el redondeo se puede realizar usando varias funciones como `roundToInt()`, `roundToDouble()`, y usando `BigDecimal` para un mayor control:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Salida: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Salida: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Salida: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Salida: 123.5
}
```

## Profundización

Históricamente, redondear números ha sido un concepto fundamental tanto en matemáticas como en computación, diseñado para manejar limitaciones de precisión numérica. En la computación temprana, el redondeo era crítico debido al alto costo de la memoria.

En Kotlin, el redondeo se basa en las bibliotecas estándar de Java. Las opciones para redondear incluyen `Math.round()`, que redondea al número entero más cercano, y `BigDecimal` para un redondeo personalizable, donde puedes especificar una escala y un `RoundingMode`.

Cada `RoundingMode` tiene diferentes políticas para manejar empates (cuando el dígito está exactamente en medio de las opciones para redondear). Por ejemplo, `RoundingMode.HALF_UP` redondea al vecino más cercano, a menos que ambos vecinos estén equidistantes, en cuyo caso redondea hacia arriba.

## Ver También

- Documentación de Kotlin sobre [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Documentación de Java de Oracle para [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- Estándar IEEE para Aritmética de Punto Flotante (IEEE 754) [Estándar IEEE 754](https://ieeexplore.ieee.org/document/4610935)
