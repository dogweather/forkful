---
title:                "Kotlin: Calculando una fecha en el futuro o pasado"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, necesitamos trabajar con fechas en nuestro código, ya sea para mostrar información en un formato específico o para realizar cálculos basados en ellas. En este caso, calcular una fecha en el futuro o en el pasado puede ser una tarea importante y útil.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Kotlin, podemos usar la clase ```java.time.LocalDate```, que forma parte del paquete ```java.time```.

Primero, necesitamos importar esta clase en nuestro archivo de Kotlin:

```Kotlin
import java.time.LocalDate
```

Luego, podemos crear un objeto ```LocalDate``` con la fecha actual utilizando el método ```now```:

```Kotlin
val fechaActual = LocalDate.now()
```

Para calcular una fecha en el futuro, podemos usar el método ```plusDays``` y especificar el número de días que queremos sumar a la fecha actual. Por ejemplo, para obtener la fecha dentro de 5 días:

```Kotlin
val fechaFutura = fechaActual.plusDays(5)
```

Del mismo modo, para calcular una fecha en el pasado, podemos usar el método ```minusDays``` y especificar el número de días que queremos restar a la fecha actual. Por ejemplo, para obtener la fecha hace 2 semanas:

```Kotlin
val fechaPasada = fechaActual.minusDays(14)
```

Podemos imprimir estas fechas en el formato que queramos utilizando el método ```format``` y especificando un formato de fecha personalizado. Por ejemplo, para imprimir la fecha en formato DD/MM/YYYY:

```Kotlin
println(fechaActual.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")))
```

## Inmersión profunda

Mientras que en este ejemplo hemos utilizado el tipo de dato ```LocalDate``` para calcular fechas en el futuro y en el pasado, también podemos utilizar otras clases del paquete ```java.time``` para realizar cálculos más complejos. Algunas opciones incluyen ```LocalDateTime```, ```Period``` y ```ChronoUnit```. Te animamos a que experimentes con estas clases y descubras qué más puedes hacer con ellas.

## Ver también

- [Documentación de la clase LocalDate en la API de Kotlin] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/) 
- [Tutorial de uso de LocalDate en Kotlin] (https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)