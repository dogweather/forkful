---
title:    "Kotlin: Comparación de dos fechas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué 
Comparar fechas es una tarea común en programación, ya sea para determinar la antigüedad de un evento o para realizar operaciones con fechas en una aplicación. En este artículo, aprenderás cómo comparar dos fechas en Kotlin de manera sencilla y eficiente.

## Cómo hacerlo 
La clase de Kotlin `LocalDate` proporciona métodos útiles para comparar fechas. Aquí hay un ejemplo de cómo comparar dos fechas y determinar si una es anterior, igual o posterior a la otra:

```Kotlin
val fecha1 = LocalDate.of(2021, 2, 10)
val fecha2 = LocalDate.parse("2021-03-15")
if (fecha1.isBefore(fecha2)) {
    println("Fecha1 es anterior a Fecha2")
} else if (fecha1.isEqual(fecha2)) {
    println("Fecha1 es igual a Fecha2")
} else {
    println("Fecha1 es posterior a Fecha2")
}
```

La salida de este código sería: `Fecha1 es anterior a Fecha2`.
También podemos utilizar el método `compareTo` para comparar dos fechas y obtener un valor numérico que indica la relación entre ellas:

```Kotlin
val resultado = fecha1.compareTo(fecha2)
if (resultado > 0) {
    println("Fecha1 es posterior a Fecha2")
} else if (resultado == 0) {
    println("Fecha1 es igual a Fecha2")
} else {
    println("Fecha1 es anterior a Fecha2")
}
```

La salida para este ejemplo sería: `Fecha1 es anterior a Fecha2`.

## Profundizando
La clase `LocalDate` también tiene métodos para realizar comparaciones basadas en diferentes unidades de tiempo, como días, semanas o años. Por ejemplo, el método `until` permite obtener la diferencia entre dos fechas en días, semanas o años:

```Kotlin
val dias = fecha1.until(fecha2, ChronoUnit.DAYS)
val semanas = fecha1.until(fecha2, ChronoUnit.WEEKS)
val anos = fecha1.until(fecha2, ChronoUnit.YEARS)
println("La diferencia en días es: ${dias}")
println("La diferencia en semanas es: ${semanas}")
println("La diferencia en años es: ${anos}")
```

La salida sería: 
```
La diferencia en días es: 33
La diferencia en semanas es: 4
La diferencia en años es: 0
```

También podemos utilizar los operadores `+` y `-` junto con los métodos `plus` y `minus` para modificar una fecha y compararla con otra:

```Kotlin
val fechaModificada = fecha1.plus(Period.ofDays(10))
println("La fecha modificada es: ${fechaModificada}")
if (fechaModificada > fecha2) {
    println("Fecha modificada es posterior a Fecha2")
} else if (fechaModificada == fecha2) {
    println("Fecha modificada es igual a Fecha2")
} else {
    println("Fecha modificada es anterior a Fecha2")
}
```

La salida en este caso sería: `La fecha modificada es: 2021-02-20` y `Fecha modificada es anterior a Fecha2`.

## Ver también
- [Documentación oficial de la clase LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Tutoriales de Kotlin en español](https://kotlinlang.org/es/docs/tutorials/)
- [Ejemplos de Kotlin en la página oficial de GitHub](https://github.com/JetBrains/kotlin-examples)

¡Ahora ya sabes cómo comparar dos fechas en Kotlin! Utiliza estos métodos en tus proyectos y recuerda revisar la documentación oficial para más información y ejemplos. ¡Hasta la próxima!