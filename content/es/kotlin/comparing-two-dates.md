---
title:                "Kotlin: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en Kotlin

Al trabajar con fechas en nuestros programas, a menudo nos encontramos con la necesidad de compararlas. Esto puede ser útil para determinar si una fecha es anterior o posterior a otra, o para calcular la diferencia de días entre dos fechas. En este artículo, veremos cómo podemos hacer esto usando el lenguaje de programación Kotlin.

## Cómo hacerlo en Kotlin

En Kotlin, podemos comparar dos fechas utilizando los operadores de comparación estándar "menor que" ("<"), "igual a" ("==") y "mayor que" (">"). También podemos utilizar los métodos `compareTo()` y `equals()` para realizar la comparación. Veamos un ejemplo de cómo comparar dos fechas utilizando estos métodos:

```
val fecha1 = LocalDate.of(2021, 2, 5)
val fecha2 = LocalDate.of(2021, 3, 15)

if (fecha1.compareTo(fecha2) < 0) {
    println("La fecha1 es anterior a la fecha2")
} else if (fecha1.compareTo(fecha2) > 0) {
    println("La fecha1 es posterior a la fecha2")
} else {
    println("Las fechas son iguales")
}
```

En este ejemplo, creamos dos objetos `LocalDate` que representan dos fechas diferentes y luego utilizamos el método `compareTo()` para compararlas. Como resultado, obtenemos un valor entero que indica si la fecha1 es anterior, posterior o igual a la fecha2. En este caso, el resultado es -1, lo que significa que fecha1 es anterior a fecha2. También podríamos haber utilizado el operador "<" en lugar del método `compareTo()`, ya que Kotlin permite comparar objetos `LocalDate` directamente.

Otra forma de comparar fechas en Kotlin es utilizando el método `equals()`. Este método devuelve un valor booleano que indica si las dos fechas son iguales o no. Veamos un ejemplo:

```
val fecha1 = LocalDate.of(2021, 2, 5)
val fecha2 = LocalDate.of(2021, 2, 5)

if (fecha1.equals(fecha2)) {
    println("Las fechas son iguales")
} else {
    println("Las fechas son diferentes")
}
```

En este caso, el resultado será "Las fechas son iguales" ya que ambas fechas representan el mismo día en el mismo año.

Podemos aplicar estas mismas técnicas para comparar fechas con precisión de horas y minutos utilizando los objetos `LocalDateTime` y `ZonedDateTime`.

## Profundizando en la comparación de fechas

Kotlin proporciona una API completa para trabajar con fechas y realizar comparaciones entre ellas. Puedes consultar la documentación oficial para obtener más información sobre cómo utilizar los operadores de comparación y los métodos `compareTo()` y `equals()` con otros tipos de datos como `LocalDateTime`, `ZonedDateTime` y `Instant`.

También es importante tener en cuenta que al comparar fechas, debemos tener en cuenta la zona horaria en la que se encuentran las fechas. Esto es especialmente importante cuando trabajamos con fechas y horas en diferentes zonas horarias.

## Ver también

- [Documentación oficial de Kotlin sobre comparar fechas](https://kotlinlang.org/docs/comparisons.html#comparison-of-dates)
- [Kotlin Date and Time API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html)