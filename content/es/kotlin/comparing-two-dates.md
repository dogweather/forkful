---
title:                "Comparando dos fechas"
html_title:           "Kotlin: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comparar dos fechas es una tarea común en la programación. Es el proceso de comparar dos valores de fecha y determinar si uno es mayor, menor o igual al otro. Los programadores lo hacen para poder realizar operaciones lógicas basadas en fechas, como verificar la fecha de caducidad de un producto o calcular la edad de una persona.

## Cómo hacerlo:

Para comparar dos fechas en Kotlin, podemos usar el método `compareTo()` disponible en la clase `LocalDate`. Este método devuelve un valor negativo si la fecha es anterior, cero si son iguales, o un valor positivo si es posterior a la otra fecha.

```Kotlin
val date1 = LocalDate.of(2021, 5, 15)
val date2 = LocalDate.now()

println(date1.compareTo(date2)) // Output: -1
```

También podemos usar los operadores de comparación (`<`, `>`, `==`) para comparar fechas en Kotlin.

```Kotlin
val date1 = LocalDate.of(2021, 5, 15)
val date2 = LocalDate.now()

println(date1 < date2) // Output: true
```

## Inmersión profunda:

En el pasado, comparar fechas en programación era una tarea complicada debido a la diversidad de formatos de fecha y hora utilizados en diferentes países y culturas. Sin embargo, con el estándar ISO 8601 y la introducción de clases de fecha y hora en los lenguajes de programación modernos, como Kotlin, se ha facilitado mucho la comparación de fechas.

Además de `compareTo()`, también podemos usar el método `isBefore()` o `isAfter()` para comparar fechas en Kotlin. También existen clases como `LocalDateTime` y `ZonedDateTime` que nos permiten comparar fechas y horas juntas.

Otras alternativas para comparar fechas incluyen el uso de librerías externas, como Joda-Time y java.time de Java.

Para implementar la comparación de fechas de manera más precisa, también podemos considerar factores como la zona horaria y el horario de verano.

## Véase también:

- Tutorial de Kotlin sobre fechas y horas: https://kotlinlang.org/docs/datetime.html
- Estándar ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
- Librería Joda-Time: https://www.joda.org/joda-time/
- Clase java.time de Java: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html